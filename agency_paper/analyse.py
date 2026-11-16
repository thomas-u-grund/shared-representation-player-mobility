"""Fit conditional destination logits to the archived player panel."""
import json
import re
import sys
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.sparse import csr_matrix, hstack
from scipy.stats import norm

root = Path(sys.argv[1])
# 'extended' (default, main sample): player-level linkage, carry-forward labels, loan-to-buy conversions excluded.
# 'archived': the first-version sample (mover must be on the origin club's roster in the baseline release).
# Sensitivity variants: 'extended_base' (no carry-forward labels), 'extended_post' (adds look-ahead labels from later releases).
sample = sys.argv[2] if len(sys.argv) > 2 else 'extended'
assert sample == 'archived' or sample.startswith('extended')
perf = root / ('perf' if sample == 'archived' else f'perf_{sample}')
anchor = 'origin_club' if sample == 'archived' else 'snapshot_club'

def read(path):
    return pd.read_csv(path, dtype={'player_id':str,'club_id':str,'candidate_club':str,'origin_club':str,'snapshot_club':str,'destination_club':str}, low_memory=False)
def key(s):
    return re.sub('[^a-z0-9]', '', unicodedata.normalize('NFKD', str(s).casefold()).encode('ascii','ignore').decode())

players = read(perf/'player_performance.csv').fillna({'agent_label':'','nationality':''})
players['label'] = players.agent_label.map(key)
players['nation'] = players.nationality.map(key)
e = read(perf/'transfer_performance.csv')
e = e[e.known_agent].merge(players[['summer','player_id','club_id','label','nation','market_value_eur']], left_on=['summer','player_id',anchor],right_on=['summer','player_id','club_id'],validate='many_to_one')
if sample != 'archived':
    e = e[e.snapshot_club.ne(e.destination_club)]   # loan-to-buy conversions: the loan, not a search, fixed the destination
c = read(perf/'within_league_candidates.csv').merge(e,on=['event_id','summer'],validate='many_to_one')
assert not c.duplicated(['event_id','candidate_club']).any()
# Build directional club histories from the full transaction file, not the agency-linked sample.
history = pd.read_csv(root/'transfers.gz', usecols=['player_id','transfer_date','from_club_id','to_club_id','transfer_fee'],
                      dtype={'player_id':str,'from_club_id':str,'to_club_id':str,'transfer_date':str})
history = history.dropna(subset=['player_id','transfer_date','from_club_id','to_club_id'])
history = history[history.from_club_id.ne(history.to_club_id) & history.transfer_date.lt('2025-06-01')]
record_key = ['player_id','transfer_date','from_club_id','to_club_id']
history['positive_fee'] = history.transfer_fee.fillna(0).gt(0)
history = history.groupby(record_key,as_index=False).positive_fee.max()
previous = history.groupby(['player_id','from_club_id','to_club_id']).transfer_date.agg(list).to_dict()
def likely_return(row):
    opposite = previous.get((row.player_id,row.to_club_id,row.from_club_id), ())
    lower = f'{int(row.transfer_date[:4])-1}-06-01'
    return any(lower <= d < row.transfer_date for d in opposite)

return_candidates = history[~history.positive_fee & history.transfer_date.str[-5:].isin(['06-30','07-01'])]
history = history.drop(index=[row.Index for row in return_candidates.itertuples() if likely_return(row)])

def add_flows(frame, windows, through):
    frame = frame.copy()
    for years in windows:
        for col in ('forward','reverse','own_history'):
            frame[f'{col}_{years}'] = 0.0
        for year, subset in frame.groupby('summer'):
            past = through[through.transfer_date.ge(f'{year-years}-06-01') & through.transfer_date.lt(f'{year}-06-01')]
            assert past.empty or past.transfer_date.max() < f'{year}-06-01'
            pair = past.groupby(['from_club_id','to_club_id']).size()
            player_pair = past.groupby(['from_club_id','to_club_id','player_id']).size()
            own = np.zeros(len(subset))
            for col, endpoints in [('forward',['origin_club','candidate_club']),('reverse',['candidate_club','origin_club'])]:
                counts = pair.reindex(pd.MultiIndex.from_frame(subset[endpoints])).fillna(0).to_numpy()
                focal = player_pair.reindex(pd.MultiIndex.from_frame(subset[endpoints+['player_id']])).fillna(0).to_numpy()
                assert (counts >= focal).all()
                frame.loc[subset.index,f'{col}_{years}'] = np.log1p(counts-focal)
                own += focal
            frame.loc[subset.index,f'own_history_{years}'] = (own > 0).astype(float)
    return frame

c = add_flows(c,[1,3,5],history)
c['any_prior_flow'] = (c.forward_3.gt(0) | c.reverse_3.gt(0)).astype(int)
c['same_prior_flow'] = c.co_clients.gt(0)*c.any_prior_flow
known = players.loc[players.label.ne('')].groupby(['summer','club_id']).player_id.nunique()
nat = players[players.nation.ne('')].groupby(['summer','club_id','nation']).player_id.nunique()
nat_total = players[players.nation.ne('')].groupby(['summer','club_id']).player_id.nunique()
c['known'] = known.reindex(pd.MultiIndex.from_frame(c[['summer','candidate_club']])).fillna(0).to_numpy()
c['nat_peers'] = nat.reindex(pd.MultiIndex.from_frame(c[['summer','candidate_club','nation']])).fillna(0).to_numpy()
c['nat_total'] = nat_total.reindex(pd.MultiIndex.from_frame(c[['summer','candidate_club']])).fillna(0).to_numpy()
c['nationality_share_10pp'] = 10*c.nat_peers/c.nat_total.replace(0,np.nan)
c['log_known'] = np.log1p(c.known)
c['same'] = c.co_clients.gt(0).astype(int)
c['co_client_count_log'] = np.log1p(c.co_clients)
c['between'] = c.route.eq('between')
c['same_between'] = c.same*c.between
c['same_2024'] = c.same*c.summer.eq(2024)
c['same_2025'] = c.same*c.summer.eq(2025)
c['turn_of_july'] = c.date.str[-5:].isin(['06-30','07-01'])
c['same_turn_of_july'] = c.same*c.turn_of_july
c['ppg_rank'] = c.candidate_ppg_percentile.fillna(0.5)
c['ppg_missing'] = c.candidate_ppg_percentile.isna().astype(int)
c['position_count'] = np.log1p(c.same_position_players.fillna(0))
c['position_minutes'] = np.log1p(c.same_position_minutes.fillna(0))
c['position_unknown'] = c.same_position_players.isna().astype(int)
c['position_minutes_unknown'] = c.same_position_minutes.isna().astype(int)
c['rating_gap'] = c.player_minus_median_rating.fillna(0)/10
c['rating_gap_unknown'] = c.player_minus_median_rating.isna().astype(int)
c['elo100'] = (c.candidate_club_elo.fillna(1500)-1500)/100
c['elo_missing'] = c.candidate_club_elo.isna().astype(int)
c['value_elo'] = c.elo100*np.log1p(c.market_value_eur.fillna(0)/1e6)
c['minutes_elo'] = c.elo100*np.log1p(c.player_minutes.fillna(0)/900)
c['rating_elo'] = c.elo100*(c.player_ea_overall.fillna(65)-65)/10
c['value_unknown_elo'] = c.elo100*c.market_value_eur.isna()
c['rating_unknown_elo'] = c.elo100*c.player_ea_overall.isna()
c['nationality_unknown'] = c.nationality_share_10pp.isna().astype(int)
c['nationality_share_10pp'] = c.nationality_share_10pp.fillna(0)
c = c[c.candidate_club.ne(c.origin_club)].copy()
c['club_summer'] = c.candidate_club+'_'+c.summer.astype(str)
c['roster_unavailable'] = c.known.eq(0).astype(int)
all_clubs = c.copy()
c = c[c.known.gt(0)].copy()

def valid(x):
    flag=x.groupby('event_id').chosen.agg(['size','sum'])
    return x[x.event_id.isin(flag.index[(flag['sum']==1)&(flag['size']>=2)])].sort_values('event_id',kind='stable').reset_index(drop=True)

def dummy(x, col, block):
    level=x[[col,block]].drop_duplicates().sort_values([block,col])
    assert not level[col].duplicated().any()
    keep=level.loc[level.duplicated(block),col]
    ids=pd.Categorical(x[col],categories=keep).codes
    idx=np.flatnonzero(ids>=0)
    return csr_matrix((np.ones(len(idx)),(idx,ids[idx])),shape=(len(x),len(keep)))

records=[]; summaries={}
def fit(name, x, cols, fixed=None, block=None, metrics=None, prune_fixed=True):
    x=valid(x)
    if fixed and prune_fixed:
        # With unrestricted fixed effects, never-chosen levels otherwise tend to minus infinity.
        while True:
            n=len(x)
            x=valid(x[x[fixed].isin(x.loc[x.chosen.eq(1),fixed])])
            if n==len(x):break
    g=x.groupby('event_id',sort=False).ngroup().to_numpy()
    start=np.flatnonzero(np.r_[True,g[1:]!=g[:-1]])
    spec=x[cols].to_numpy(dtype=float)
    varying=np.any(np.abs(spec-spec[start][g])>1e-9,axis=0)
    cols=list(np.asarray(cols)[varying])
    X=csr_matrix(spec[:,varying])
    if fixed:X=hstack([X,dummy(x,fixed,block)],format='csr')
    y=x.chosen.to_numpy(dtype=float)
    def calc(b):
        v=X@b
        vmax=np.maximum.reduceat(v,start)
        ev=np.exp(v-vmax[g])
        total=np.add.reduceat(ev,start)
        return np.sum(np.log(total)+vmax)-y@v,ev/total[g]
    def obj(b):
        ll,p=calc(b)
        return ll,np.asarray(X.T@(p-y)).ravel()
    sol=minimize(obj,np.zeros(X.shape[1]),jac=True,method='L-BFGS-B',options={'gtol':1e-8,'ftol':1e-13,'maxiter':20000})
    assert np.max(np.abs(obj(sol.x)[1]))<0.02,(name,sol.message,obj(sol.x)[1])
    ll,prob=calc(sol.x)
    membership=csr_matrix((prob,(g,np.arange(len(x)))),shape=(len(start),len(x)))
    means=membership@X
    H=(X.T@X.multiply(prob[:,None])-means.T@means).toarray()
    inv=np.linalg.pinv(H,hermitian=True)
    targets = {col:{col:1} for col in cols if col in ('same','co_client_count_log','same_between','same_2024','same_2025','agency_log','local_nationality','forward_3','reverse_3','same_prior_flow','same_turn_of_july')}
    targets.update(metrics or {})
    vc={}
    for cluster in ('player_id','label'):
        groups=pd.factorize(x[cluster],sort=False)[0]
        agg=csr_matrix((y-prob,(groups,np.arange(len(x)))),shape=(groups.max()+1,len(x)))@X
        v=agg@inv[:,:len(cols)]
        vc[cluster]=(v.T@v)*len(v)/(len(v)-1)
    for term,parts in targets.items():
        w=np.array([parts.get(col,0) for col in cols])
        b=w@sol.x[:len(cols)]
        rec=dict(model=name,term=term,events=len(start),alternatives=len(x),coefficient=b,odds_ratio=np.exp(b))
        for cluster,V in vc.items():
            se=np.sqrt(w@V@w);rec[f'{cluster}_se']=se
            rec[f'{cluster}_lo']=np.exp(b-1.96*se);rec[f'{cluster}_hi']=np.exp(b+1.96*se)
            rec[f'{cluster}_p']=2*norm.sf(abs(b/se))
        records.append(rec)
    chose=x[x.chosen.eq(1)]
    exposed=x.groupby('event_id').same.any() if 'same' in x else None
    summary=dict(events=len(start),alternatives=len(x),players=int(chose.player_id.nunique()),labels=int(chose.label.nunique()),neg_loglik=float(ll))
    if exposed is not None:
        summary.update(exposed_events=int(exposed.sum()),chosen_exposed=int(chose.same.sum()),candidate_share=float(x.same.mean()))
        if 'same' in cols:
            xb=X@sol.x
            xb0=xb.copy()
            for col in ('same','same_between','same_2024','same_2025','same_prior_flow','same_turn_of_july'):
                if col in cols: xb0-=x[col].to_numpy()*sol.x[cols.index(col)]
            ex=x.same.to_numpy()
            p0=np.exp(xb0-np.maximum.reduceat(xb0,start)[g])
            p0/=np.add.reduceat(p0,start)[g]
            summary.update(predicted_exposed_given_eligible=float((prob*ex).sum()/exposed.sum()),
                           equalised_exposed_given_eligible=float((p0*ex).sum()/exposed.sum()))
    if 'route' in chose:
        summary['routes']={k:int(v) for k,v in chose.route.value_counts().items()}
    if 'any_prior_flow' in x:
        summary['prior_flow_chosen']=int(chose.any_prior_flow.sum())
        summary['prior_flow_alternative_share']=float(x.any_prior_flow.mean())
    if 'turn_of_july' in chose:
        summary['events_on_30_june_or_1_july']=int(chose.turn_of_july.sum())
    summaries[name]=summary
    print(name,summary,flush=True)

main=c[~c.likely_return]
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
fit('Club baseline',main,['same','log_known'])
fit('Club adjusted',main,['same']+controls)
fit('Club fixed effects',main,['same']+controls,'candidate_club','candidate_league')
fit('Club by summer fixed effects',main,['same']+controls,'club_summer','candidate_league')
flows=['forward_3','reverse_3','own_history_3']
fit('Club past flows',main,['same']+controls+flows,'candidate_club','candidate_league')
fit('Club by summer past flows',main,['same']+controls+flows,'club_summer','candidate_league')
fit('Club short past flows',main,['same']+controls+['forward_1','reverse_1','own_history_1'],'candidate_club','candidate_league')
fit('Club long past flows',main,['same']+controls+['forward_5','reverse_5','own_history_5'],'candidate_club','candidate_league')
fit('Club past flows excluding turn of July',main[~main.date.str[-5:].isin(['06-30','07-01'])],['same']+controls+flows,'candidate_club','candidate_league')
fit('Club timing interaction',main,['same','same_turn_of_july']+controls+flows,'candidate_club','candidate_league',
    {'shared label on 30 June or 1 July':{'same':1,'same_turn_of_july':1}})
fit('Club past flows interaction',main,['same','same_prior_flow']+controls+flows+['any_prior_flow'],'candidate_club','candidate_league',
    {'shared label with past flows':{'same':1,'same_prior_flow':1}})
fit('Club including unobserved rosters',all_clubs[~all_clubs.likely_return],['same','roster_unavailable']+controls,'candidate_club','candidate_league')
fit('Club broad sample',c,['same']+controls,'candidate_club','candidate_league')
fit('Club excluding turn of July',main[~main.date.str[-5:].isin(['06-30','07-01'])],['same']+controls,'candidate_club','candidate_league')
fit('Club player quality by Elo',main,['same']+controls+['value_elo','minutes_elo','rating_elo','value_unknown_elo','rating_unknown_elo'],'candidate_club','candidate_league')
fit('Club dose',main,['co_client_count_log']+controls,'candidate_club','candidate_league')
fit('Club route interaction',main,['same','same_between']+controls,'candidate_club','candidate_league',
    {'between league total':{'same':1,'same_between':1}})
fit('Club year interaction',main,['same','same_2024','same_2025']+controls,'candidate_club','candidate_league',
    {'2024 total':{'same':1,'same_2024':1},'2025 total':{'same':1,'same_2025':1}})

countries=dict(zip('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1'.split(),'Belgium Denmark Spain France England Greece Italy Germany Netherlands Portugal Russia Scotland Turkey Ukraine'.split()))
counts=players[players.label.ne('')].groupby(['summer','league','label']).size()
count_known=players[players.label.ne('')].groupby(['summer','league']).size()
e['snapshot_league']=players.set_index(['summer','player_id','club_id']).league.reindex(pd.MultiIndex.from_arrays([e.summer,e.player_id,e[anchor]])).to_numpy()
league=e[~e.likely_return].assign(tmp=1).merge(pd.DataFrame({'alternative':list(countries),'tmp':1}),on='tmp')
league['chosen']=league.destination_league.eq(league.alternative).astype(int)
league['agency_log']=np.log1p(np.maximum(0,counts.reindex(pd.MultiIndex.from_frame(league[['summer','alternative','label']])).fillna(0).to_numpy()-league.alternative.eq(league.snapshot_league)))
league['log_roster']=np.log(count_known.reindex(pd.MultiIndex.from_frame(league[['summer','alternative']])).to_numpy())
league['local_nationality']=league.nation.eq(league.alternative.map(countries).map(key)).astype(int)
league.loc[league.alternative.eq('TR1') & league.nation.eq('turkiye'),'local_nationality']=1
league['route_fe']=league.origin_league+'_'+league.alternative
fit('League fixed routes',league,['agency_log','log_roster','local_nationality'],'route_fe','origin_league',prune_fixed=True)
lc=league[league.route.eq('between') & league.origin_league.ne(league.alternative)]
fit('League fixed cross routes',lc,['agency_log','log_roster','local_nationality'],'route_fe','origin_league',prune_fixed=True)
pd.DataFrame(records).to_csv('model_results.csv',index=False)
Path('sample_summary.json').write_text(json.dumps(summaries,indent=2))
