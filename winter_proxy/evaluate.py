"""Backcast February rosters and benchmark January destination models."""
import argparse
import json
import re
import unicodedata
import zipfile
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.sparse import csr_matrix

parser = argparse.ArgumentParser()
parser.add_argument('snapshots', type=Path, help='Kaggle download bodies data_VERSION')
parser.add_argument('matches', type=Path, help='games.gz and transfers.gz')
parser.add_argument('observed_winter', type=Path, help='Recovered December winter extract')
args = parser.parse_args()
sources = pd.read_csv('proxy_sources.csv', dtype={'version':str})
leagues = set('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1'.split())

def key(s):
    s = str(s).strip().casefold()
    if s in ('', 'none', 'nan', 'unknown', 'without agent', 'no agent', 'null', 'n/a', '-'):
        return ''
    return re.sub('[^a-z0-9]', '', unicodedata.normalize('NFKD',s).encode('ascii','ignore').decode())

g = pd.read_csv(args.matches/'games.gz',low_memory=False)
members = defaultdict(dict)
for r in g[g.competition_id.isin(leagues)].itertuples():
    for c in (r.home_club_id,r.away_club_id):
        members[r.season][c]=r.competition_id
raw = pd.read_csv(args.matches/'transfers.gz', low_memory=False)
raw = raw.dropna(subset=['player_id','transfer_date','from_club_id','to_club_id'])
raw = raw[raw.from_club_id.ne(raw.to_club_id)].copy()
raw['positive_fee'] = raw.transfer_fee.fillna(0).gt(0)
moves = raw.groupby(['player_id','transfer_date','from_club_id','to_club_id'],as_index=False).positive_fee.max()
moves[['player_id','from_club_id','to_club_id']] = moves[['player_id','from_club_id','to_club_id']].astype(int)
routes_by_player = moves.groupby(['player_id','from_club_id','to_club_id']).transfer_date.agg(list).to_dict()
obs = pd.read_csv(args.observed_winter/'winter_player_snapshots.csv').fillna({'agent_label':'','agent_key':''})

def backcast(current, groups):
    """Reverse a unique chain on each date, newest date first."""
    for _, edges in sorted(groups.items(),reverse=True):
        edges = list(edges)
        while edges:
            matching = [i for i,(_,b) in enumerate(edges) if b == current]
            if len(matching) != 1:
                return None
            current,_ = edges.pop(matching[0])
    return current

def january_moves(year,clubs):
    z = moves[moves.transfer_date.between(f'{year}-01-01',f'{year}-01-31') &
              moves.from_club_id.isin(clubs) & moves.to_club_id.isin(clubs)].copy()
    z['january'] = year
    z['event_id'] = [f'W{year}-{i:04}' for i in range(1,len(z)+1)]
    z['reverse_flag'] = [
        not x.positive_fee and any(f'{year-1}-01-01' <= d < x.transfer_date
            for d in routes_by_player.get((x.player_id,x.to_club_id,x.from_club_id),()))
        for x in z.itertuples()]
    return z

def make_candidates(z,roster,clubs):
    labels = {(x.player_id,x.club_id):x.agent_key for x in roster.itertuples() if x.agent_key}
    sizes = Counter(b for _,b in labels)
    clients = Counter((b,a) for (_,b),a in labels.items())
    result=[]
    for x in z.itertuples():
        a=labels.get((x.player_id,x.from_club_id))
        if not a or not sizes[x.to_club_id]:
            continue
        pool=[b for b,l in clubs.items() if l==clubs[x.to_club_id] and b!=x.from_club_id and sizes[b]]
        if len(pool)<2:
            continue
        for b in sorted(pool):
            n=clients[b,a]-int(labels.get((x.player_id,b))==a)
            result.append(dict(event_id=x.event_id,january=x.january,player_id=x.player_id,
                origin_club=x.from_club_id,candidate_club=b,chosen=int(b==x.to_club_id),agent_key=a,
                reverse_flag=x.reverse_flag,known_roster_labels=sizes[b],co_clients=n))
    return pd.DataFrame(result)

def fit(frame, name, specification):
    x=frame.copy()
    if specification=='flow_adjusted':
        while True:
            n=len(x)
            x=x[x.candidate_club.isin(x.loc[x.chosen.eq(1),'candidate_club'])]
            sizes=x.groupby('event_id').chosen.agg(['size','sum'])
            x=x[x.event_id.isin(sizes.index[sizes['size'].ge(2)&sizes['sum'].eq(1)])]
            if len(x)==n:break
    if x.empty:return []
    x=x.sort_values('event_id')
    g=pd.factorize(x.event_id)[0]
    start=np.flatnonzero(np.r_[True,g[1:]!=g[:-1]])
    y=x.chosen.to_numpy()
    assert np.add.reduceat(y,start).min()==np.add.reduceat(y,start).max()==1
    columns=[x.co_clients.gt(0),np.log1p(x.known_roster_labels)]
    if specification=='flow_adjusted':
        columns += [x.forward,x.reverse]
    z=csr_matrix(np.column_stack(columns).astype(float))
    if specification=='flow_adjusted':
        from scipy.sparse import hstack
        z=hstack([z,csr_matrix(pd.get_dummies(x.candidate_club,dtype=float).to_numpy())],format='csr')
    def probabilities(b):
        v=z@b
        vmax=np.maximum.reduceat(v,start)
        ex=np.exp(v-vmax[g]); totals=np.add.reduceat(ex,start)
        return ((np.log(totals)+vmax).sum()-y@v,ex/totals[g])
    def objective(b):
        ll,p=probabilities(b)
        return ll,np.asarray(z.T@(p-y)).ravel()
    sol=minimize(objective,np.zeros(z.shape[1]),jac=True,method='L-BFGS-B',
                 options={'ftol':1e-13,'gtol':1e-9,'maxiter':1600})
    assert np.max(np.abs(objective(sol.x)[1]))<.02,(name,specification,sol.message)
    _,p=probabilities(sol.x)
    m=csr_matrix((p,(g,np.arange(len(x)))),shape=(len(start),len(x)))@z
    h=(z.T@z.multiply(p[:,None])-m.T@m).toarray()
    influence=np.linalg.pinv(h,hermitian=True)[:,0]
    out=[]
    for col in ('agent_key','player_id'):
        group=pd.factorize(x[col])[0]
        s=np.bincount(group,weights=(y-p)*(z@influence))
        se=np.sqrt((s@s)*len(s)/(len(s)-1))
        out.append(dict(sample=name,specification=specification,cluster=col,events=len(start),
                        alternatives=len(x),odds_ratio=np.exp(sol.x[0]),lo=np.exp(sol.x[0]-1.96*se),
                        hi=np.exp(sol.x[0]+1.96*se)))
    return out

def flow_counts(frame):
    results=[]
    for year,x in frame.groupby('january'):
        past=moves[moves.transfer_date.ge(f'{year-3}-01-01') & moves.transfer_date.lt(f'{year}-01-01')]
        # Same conservative historical flag as the winter outcome, evaluated at each record date.
        suspect=[not r.positive_fee and any(f'{int(r.transfer_date[:4])-1}-01-01'<=d<r.transfer_date
                 for d in routes_by_player.get((r.player_id,r.to_club_id,r.from_club_id),()))
                 for r in past.itertuples()]
        past=past[~np.array(suspect)]
        pair=past.groupby(['from_club_id','to_club_id']).size()
        own=past.groupby(['from_club_id','to_club_id','player_id']).size()
        x=x.copy()
        for name,cols in [('forward',['origin_club','candidate_club']),('reverse',['candidate_club','origin_club'])]:
            a=pair.reindex(pd.MultiIndex.from_frame(x[cols])).fillna(0).to_numpy()
            b=own.reindex(pd.MultiIndex.from_frame(x[cols+['player_id']])).fillna(0).to_numpy()
            x[name]=np.log1p(a-b)
        results.append(x)
    return pd.concat(results,ignore_index=True)

summary=[]; roster_files=[]; transfers=[]; proxy_candidates=[]; actual_candidates=[]; common_actual=[]; common_proxy=[]
for s in sources.itertuples():
    year=s.january
    clubs=members[year-1]
    f=args.snapshots/('data_'+s.version)
    p=pd.read_csv(f, compression='zip' if zipfile.is_zipfile(f) else None,low_memory=False)
    assert p.player_id.is_unique
    p=p.dropna(subset=['current_club_id']).copy()
    p['agent_label']=p.agent_name.fillna('')
    p['agent_key']=p.agent_label.map(key)
    e=moves[moves.transfer_date.between(f'{year}-01-01',s.snapshot_date,inclusive='left')]
    changes=defaultdict(lambda:defaultdict(list))
    for r in e.itertuples():
        changes[r.player_id][r.transfer_date].append((r.from_club_id,r.to_club_id))
    rows=[]; unresolved=0
    for r in p.itertuples():
        current=int(r.current_club_id)
        if r.last_season != year-1 and r.player_id not in changes:
            continue
        baseline=backcast(current,changes[r.player_id]) if r.player_id in changes else current
        if baseline is None:
            if current in clubs or any(a in clubs or b in clubs for es in changes[r.player_id].values() for a,b in es):
                unresolved+=1
            continue
        if baseline in clubs:
            rows.append(dict(january=year,player_id=r.player_id,club_id=baseline,post_snapshot_club_id=current,
                league=clubs[baseline],player_name=r.name,agent_label=r.agent_label,agent_key=r.agent_key,
                traced_moves=sum(map(len,changes[r.player_id].values()))))
    roster=pd.DataFrame(rows)
    assert roster.player_id.is_unique
    z=january_moves(year,clubs)
    z=z.merge(roster[['player_id','club_id','agent_label','agent_key']],left_on=['player_id','from_club_id'],
        right_on=['player_id','club_id'],how='left',validate='many_to_one').fillna({'agent_label':'','agent_key':''})
    transfers.append(z)
    c=make_candidates(z,roster,clubs)
    roster_files.append(roster); proxy_candidates.append(c)
    row=dict(january=year,raw_moves=len(z),named_origin_matches=int(z.agent_key.ne('').sum()),
             named_origin_matches_screened=int((z.agent_key.ne('')&~z.reverse_flag).sum()),
             reconstructed_roster=len(roster),changed_club=int(roster.club_id.ne(roster.post_snapshot_club_id).sum()),
             unresolved_chains=unresolved)
    if year in (2024,2025):
        a=obs[obs.january.eq(year)]
        ap=make_candidates(z,a,clubs);actual_candidates.append(ap)
        q=a.merge(roster,on='player_id',suffixes=('_december','_proxy'),validate='one_to_one')
        both=q.agent_key_december.ne('')&q.agent_key_proxy.ne('')
        paired=ap.merge(c,on=['event_id','candidate_club'],suffixes=('_december','_proxy'),validate='one_to_one')
        paired=paired[~paired.reverse_flag_december]
        admitted=paired.groupby('event_id').chosen_december.agg(['sum','size'])
        paired=paired[paired.event_id.isin(admitted.index[admitted['sum'].eq(1)&admitted['size'].ge(2)])]
        ks=paired[['event_id','candidate_club']]
        common_actual.append(ap.merge(ks,on=['event_id','candidate_club']))
        common_proxy.append(c.merge(ks,on=['event_id','candidate_club']))
        az=ap[ap.chosen.eq(1)][['event_id','agent_key']].merge(
            c[c.chosen.eq(1)][['event_id','agent_key','reverse_flag']],on='event_id',suffixes=('_december','_proxy'))
        az=az[~az.reverse_flag]
        ax=paired.co_clients_december.gt(0); bx=paired.co_clients_proxy.gt(0)
        selected=paired.chosen_december.eq(1)
        pd.DataFrame(paired).to_csv(f'paired_validation_{year}.csv',index=False)
        row.update(december_roster=len(a),overlapping_players=len(q),
            same_club=int(q.club_id_december.eq(q.club_id_proxy).sum()),
            agent_comparable=int(both.sum()),same_agent=int((both&q.agent_key_december.eq(q.agent_key_proxy)).sum()),
            paired_screened_events=paired.event_id.nunique(),paired_candidates=len(paired),
            same_shared_indicator=int(ax.eq(bx).sum()),shared_december=int(ax.sum()),
            shared_proxy=int(bx.sum()),shared_in_both=int((ax&bx).sum()),
            mover_agent_comparable=len(az),mover_agent_same=int(az.agent_key_december.eq(az.agent_key_proxy).sum()),
            chosen_shared_december=int((ax&selected).sum()),chosen_shared_proxy=int((bx&selected).sum()),
            chosen_shared_in_both=int((ax&bx&selected).sum()))
    summary.append(row)

p_all=flow_counts(pd.concat(proxy_candidates,ignore_index=True))
a_all=flow_counts(pd.concat(actual_candidates,ignore_index=True))
a_common=flow_counts(pd.concat(common_actual,ignore_index=True))
p_common=flow_counts(pd.concat(common_proxy,ignore_index=True))
models=[]
for sample, df in [
    ('matched 2024–25 December',a_common),('matched 2024–25 backcast',p_common),
    ('2024–25 December',a_all),('2026 backcast',p_all[p_all.january.eq(2026)]),
    ('2024–25 December plus 2026 backcast',pd.concat([a_all,p_all[p_all.january.eq(2026)]]))]:
    for spec in ('roster_adjusted','flow_adjusted'):
        models+=fit(df[~df.reverse_flag],sample,spec)
pd.DataFrame(summary).to_csv('validation.csv',index=False)
pd.DataFrame(models).to_csv('winter_models.csv',index=False)
pd.concat(roster_files).to_csv('backcast_rosters.csv',index=False)
pd.concat(transfers).to_csv('backcast_january_transfers.csv',index=False)
p_all.to_csv('backcast_candidates.csv',index=False)
print(pd.DataFrame(summary).to_string(index=False))
print(pd.DataFrame(models).round(3).to_string(index=False))
