"""Harmonise winter and summer destination risk sets and fit conditional logits."""
import sys
import zipfile
from collections import defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.sparse import csr_matrix, hstack

root=Path(sys.argv[1])
sources=Path(sys.argv[2])
# 'extended' (default, main): player-level linkage, carry-forward labels, conversions excluded; 'archived': first-version sample
sample=sys.argv[3] if len(sys.argv)>3 else 'extended'
assert sample in ('extended','archived')
perf='perf_extended' if sample=='extended' else 'perf'
winter_dir='winter_recovery_extended' if sample=='extended' else 'winter_recovery'
def read(p):
    return pd.read_csv(p,low_memory=False)
raw=read(root/'coach_retry/transfers.gz').dropna(subset=['player_id','from_club_id','to_club_id','transfer_date'])
raw=raw[raw.from_club_id.ne(raw.to_club_id)].copy()
raw['positive_fee']=raw.transfer_fee.fillna(0).gt(0)
history=raw.groupby(['player_id','transfer_date','from_club_id','to_club_id'],as_index=False).positive_fee.max()
for k in ('player_id','from_club_id','to_club_id'):history[k]=history[k].astype(int)
history['date']=pd.to_datetime(history.transfer_date,format='%Y-%m-%d')
reverse=defaultdict(list)
for r in history.itertuples():reverse[r.player_id,r.to_club_id,r.from_club_id].append(r.date)
def flag(r):
    return not r.positive_fee and any(r.date-pd.Timedelta(days=365)<=d<r.date
                                     for d in reverse[r.player_id,r.from_club_id,r.to_club_id])
history['possible_return']=[flag(r) for r in history.itertuples()]
labelled=history[~history.possible_return]

def get_winter_roster(path, versions):
    r=read(path).fillna({'agent_label':'','agent_key':''})
    rows=[]
    for yr,version in versions.items():
        p=sources/f'data_{version}'
        attrs=pd.read_csv(p,compression='zip' if zipfile.is_zipfile(p) else None,
                          usecols=['player_id','country_of_citizenship'])
        rows.append(r[r.january.eq(yr)].merge(attrs,on='player_id',how='left',validate='many_to_one'))
    return pd.concat(rows).rename(columns={'country_of_citizenship':'nationality'})

def roster_covariates(c,p,anchor='origin'):
    known=p[p.agent_label.fillna('').ne('')].groupby('club_id').size()
    nat=p[p.nationality.fillna('').ne('')]
    n=nat.groupby('club_id').size()
    same=nat.groupby(['club_id','nationality']).size()
    if anchor=='player':   # label read from the mover's own roster row, wherever he is listed
        c=c.merge(p[['player_id','club_id','agent_label','nationality']],on='player_id',validate='many_to_one')
        c=c[c.club_id.ne(c.destination_club)]   # loan-to-buy conversions
    else:
        c=c.merge(p[['player_id','club_id','agent_label','nationality']],left_on=['player_id','origin_club'],
                  right_on=['player_id','club_id'],validate='many_to_one')
    c['known']=known.reindex(c.candidate_club).fillna(0).to_numpy()
    c['rsize']=np.log1p(c.known)
    base=n.reindex(c.candidate_club).fillna(0).to_numpy()
    co=same.reindex(pd.MultiIndex.from_frame(c[['candidate_club','nationality']])).fillna(0).to_numpy()
    c['nationality_missing']=(base==0)|c.nationality.isna()|c.nationality.eq('')
    c['nationality_share']=np.divide(co,base,out=np.zeros_like(co,dtype=float),where=base>0)*10
    return c[c.known.gt(0)].copy()

def annotate(c, period, yr):
    # yr is the summer calendar year; the following January is part of the same football season.
    c['season']=yr
    c['winter']=int(period=='winter')
    c['event_id']=c.event_id.astype(str).map(lambda x:f'{period[0]}{yr}-{x}')
    c['same']=c.co_clients.gt(0).astype(int)
    c['date']=pd.to_datetime(c.date)
    c['turn_of_july']=(c.date.dt.month.eq(6)&c.date.dt.day.eq(30))|(c.date.dt.month.eq(7)&c.date.dt.day.eq(1))
    keys=['player_id','date','origin_club','destination_club']
    t=history[['player_id','date','from_club_id','to_club_id','possible_return']].rename(columns={'from_club_id':'origin_club','to_club_id':'destination_club'})
    c=c.merge(t,on=keys,how='left',validate='many_to_one')
    assert c.possible_return.notna().all()
    cut=pd.Timestamp(yr,6,1) if period=='summer' else pd.Timestamp(yr+1,1,1)
    past=labelled[labelled.date.ge(cut-pd.DateOffset(years=3))&labelled.date.lt(cut)]
    pair=past.groupby(['from_club_id','to_club_id']).size()
    own=past.groupby(['from_club_id','to_club_id','player_id']).size()
    own_count=0
    for col,ends in [('forward',['origin_club','candidate_club']),('reverse',['candidate_club','origin_club'])]:
        a=pair.reindex(pd.MultiIndex.from_frame(c[ends])).fillna(0).to_numpy()
        b=own.reindex(pd.MultiIndex.from_frame(c[ends+['player_id']])).fillna(0).to_numpy()
        assert (a>=b).all()
        c[col]=np.log1p(a-b)
        own_count+=b
    c['own_history']=own_count>0
    c['label']=c.agent_label.str.casefold().str.normalize('NFKD').str.encode('ascii','ignore').str.decode('ascii').str.replace(r'[^a-z0-9]','',regex=True)
    assert not c.label.eq('').any()
    return c

sp=read(root/f'coach_retry/{perf}/player_performance.csv')
se=read(root/f'coach_retry/{perf}/transfer_performance.csv')
sc=read(root/f'coach_retry/{perf}/within_league_candidates.csv').merge(
  se[['event_id','player_id','origin_club','destination_club','date']],on='event_id',validate='many_to_one')
summer=[annotate(roster_covariates(c,sp[sp.summer.eq(y)],'player' if sample=='extended' else 'origin'),'summer',y) for y,c in sc.groupby('summer')]

def winter(path,versions,rosters,transfers,candidates,anchor='origin'):
    p=get_winter_roster(path/rosters,versions)
    e=read(path/transfers).rename(columns={'from_club_id':'origin_club','to_club_id':'destination_club','transfer_date':'date'})
    c=read(path/candidates)[['event_id','january','candidate_club','chosen','co_clients']]
    c=c.merge(e[['event_id','january','player_id','origin_club','destination_club','date']],
              on=['event_id','january'],validate='many_to_one')
    return [annotate(roster_covariates(a,p[p.january.eq(y)],anchor),'winter',y-1)
            for y,a in c.groupby('january') if y in versions]

observed=winter(root/winter_dir,{2024:325,2025:547},'winter_player_snapshots.csv','january_transfers.csv','january_destination_candidates.csv','player' if sample=='extended' else 'origin')
def backcast(path,version):
    return winter(path,{2026:version},'backcast_rosters.csv','backcast_january_transfers.csv','backcast_candidates.csv')[0]
proxy=root/'winter_proxy'
w26a=backcast(proxy,603)
w26b=backcast(proxy/'version_606_sensitivity',606)

def valid(x):
    sizes=x.groupby('event_id').chosen.agg(['size','sum'])
    return x[x.event_id.isin(sizes.index[sizes['size'].ge(2)&sizes['sum'].eq(1)])]

def fit(name,x,fixed='candidate_club'):
    x=valid(x)
    while True:
        n=len(x)
        x=valid(x[x[fixed].isin(x.loc[x.chosen.eq(1),fixed])])
        if len(x)==n:break
    x=x.sort_values('event_id')
    assert not x.duplicated(['event_id','candidate_club']).any()
    group=pd.factorize(x.event_id)[0]
    starts=np.flatnonzero(np.r_[True,group[1:]!=group[:-1]])
    y=x.chosen.to_numpy()
    cols=['same','same_winter','rsize','nationality_share','nationality_missing','forward','reverse','own_history']
    x=x.assign(same_winter=x.same*x.winter)
    cov=csr_matrix(x[cols].to_numpy(dtype=float))
    Z=hstack([cov,csr_matrix(pd.get_dummies(x[fixed],dtype=float).to_numpy())],format='csr')
    def calc(b):
        v=Z@b
        mx=np.maximum.reduceat(v,starts)
        w=np.exp(v-mx[group])
        den=np.add.reduceat(w,starts)
        return (np.log(den)+mx).sum()-y@v,w/den[group]
    def obj(b):
        ll,p=calc(b)
        return ll,np.asarray(Z.T@(p-y)).ravel()
    opt=minimize(obj,np.zeros(Z.shape[1]),jac=True,method='L-BFGS-B',options={'gtol':1e-9,'ftol':1e-13,'maxiter':20000})
    assert np.max(np.abs(obj(opt.x)[1]))<.02,(name,opt.message)
    _,p=calc(opt.x)
    M=csr_matrix((p,(group,np.arange(len(x)))),shape=(len(starts),len(x)))@Z
    H=(Z.T@Z.multiply(p[:,None])-M.T@M).toarray()
    U=np.linalg.pinv(H,hermitian=True)[:,:2]
    selected=x[x.chosen.eq(1)]
    out=[]
    for cl in ('label','player_id'):
        ids=pd.factorize(x[cl])[0]
        residual=(y-p)[:,None]*(Z@U)
        scores=np.column_stack([np.bincount(ids,weights=residual[:,k]) for k in range(2)])
        V=(scores.T@scores)*len(scores)/(len(scores)-1)
        for term,w in [('summer',[1,0]),('winter',[1,1]),('winter_to_summer',[0,1])]:
            b=w@opt.x[:2]; se=np.sqrt(w@V@w)
            out.append(dict(model=name,term=term,cluster=cl,events=len(starts),
                summer_events=int(selected.winter.eq(0).sum()),winter_events=int(selected.winter.sum()),
                odds_ratio=np.exp(b),lo=np.exp(b-1.96*se),hi=np.exp(b+1.96*se)))
    return out

primary=pd.concat(summer[:2]+observed,ignore_index=True)
primary['club_season']=primary.candidate_club.astype(str)+'_'+primary.season.astype(str)
main=primary[~primary.possible_return]
results=fit('Observed paired seasons',main)
results+=fit('Observed paired seasons club-by-season FE',main,'club_season')
results+=fit('Observed paired seasons excluding 30 June and 1 July',main[~main.turn_of_july])
results+=fit('Observed paired seasons all linked moves',primary)
for version,wr in [(603,w26a),(606,w26b)]:
    f=pd.concat([main,summer[2],wr],ignore_index=True)
    results+=fit(f'Add summer 2025 and January 2026 proxy {version}',f[~f.possible_return])
out=pd.DataFrame(results)
out.to_csv('season_comparison_results.csv',index=False)
print(out[out.cluster.eq('label')].round(3).to_string(index=False))
for s, subset in valid(main).query('chosen==1').groupby(['season','winter']):
    print('pre_fixed_effects sample',s,len(subset))
