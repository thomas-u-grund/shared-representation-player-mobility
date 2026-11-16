"""Point 2: can brokerage be told apart from segmentation?  Two empirical contrasts.

(a) Twin-agency placebo: agencies with the most similar *portfolio profile* (nationality, position, age mix),
    excluding any club information.  If sorting reflected clubs recruiting from a profile, twin presence should
    predict destinations as well as the mover's own agency does.
(b) Recently placed versus long-standing co-clients: a co-client who arrived by transfer within three years is
    a trace of a placement; a long-standing co-client (academy product, old signing) is not.
"""
import sys; sys.path.insert(0,'.')
import warnings; warnings.filterwarnings('ignore')
import numpy as np, pandas as pd
from common import *
ns = load(); ev, tr = enrich(ns)
c, fit, players = ns['c'], ns['fit'], ns['players']
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
flows=['forward_3','reverse_3','own_history_3']
D=('candidate_club','candidate_league')
main = add_event_flags(c[~c.likely_return], ev)

# ---------- (b) recent vs long-standing co-clients
tr2 = tr.dropna(subset=['player_id','to_club_id','transfer_date'])
arr = tr2.groupby(['player_id','to_club_id']).transfer_date.apply(list).to_dict()
def arrival(summer, club, pid):
    lim = f'{summer}-06-01'; ds=[d for d in arr.get((pid,club),()) if d<lim]
    return max(ds) if ds else None
pl = players[players.label.ne('')].copy()
pl['arrival'] = [arrival(s,cl,p) for s,cl,p in zip(pl.summer,pl.club_id,pl.player_id)]
pl['recent'] = [isinstance(a,str) and a>=f'{s-3}-06-01' for a,s in zip(pl.arrival,pl.summer)]
n_all = pl.groupby(['summer','club_id','label']).size()
n_rec = pl[pl.recent].groupby(['summer','club_id','label']).size()
idx = pd.MultiIndex.from_frame(main[['summer','candidate_club','label']])
main['n_all'] = n_all.reindex(idx).fillna(0).to_numpy(); main['n_rec'] = n_rec.reindex(idx).fillna(0).to_numpy()
# the focal player is on his snapshot club's roster; he never counts as his own co-client
foc = main.candidate_club.eq(main.snapshot_club).to_numpy()
rec_lookup = {(s,p,cl):r for s,p,cl,r in zip(pl.summer,pl.player_id,pl.club_id,pl.recent)}
foc_rec = np.array([rec_lookup.get((s,p,cl),False) for s,p,cl in zip(main.summer,main.player_id,main.candidate_club)])
main['n_all'] = main.n_all - foc.astype(int); main['n_rec'] = main.n_rec - (foc & foc_rec).astype(int)
assert (main.n_all==main.co_clients).mean()>0.995, (main.n_all==main.co_clients).mean()
main['same_recent'] = (main.n_rec>0).astype(int)
main['same_old'] = ((main.n_all>0)&(main.n_rec==0)).astype(int)

# ---------- (a) twin agencies
pl['pos'] = pl.position.fillna('NA'); pl['ageband']=pd.cut(pl.age_at_cutoff,[0,21,25,29,99],labels=False).astype('Int64').astype(str)
twin_of = {}
for s, d in pl.groupby('summer'):
    n = d.groupby('label').size()
    feats = pd.concat([pd.crosstab(d.label,d.nation.replace('','NA')).pipe(lambda x:x.div(x.sum(1),axis=0)),
                       pd.crosstab(d.label,d.pos).pipe(lambda x:x.div(x.sum(1),axis=0)),
                       pd.crosstab(d.label,d.ageband).pipe(lambda x:x.div(x.sum(1),axis=0))],axis=1)
    F = feats.to_numpy(); F = F/np.linalg.norm(F,axis=1,keepdims=True); labs = feats.index.to_numpy(); nn = n.reindex(labs).to_numpy()
    S = F@F.T; np.fill_diagonal(S,-1)
    for i,l in enumerate(labs):
        if nn[i] < 3: continue
        ok = (nn>=max(4,0.5*nn[i]))&(nn<=2*nn[i]); ok[i]=False
        if ok.sum()<3: continue
        cand = np.flatnonzero(ok); top = cand[np.argsort(-S[i,cand])[:3]]
        twin_of[(s,l)] = list(labs[top])
print('focal labels with twins:',len(twin_of))
cnt = pl.groupby(['summer','club_id','label']).size().to_dict()
def twin_any(r):
    tw = twin_of.get((r.summer,r.label))
    if tw is None: return np.nan
    return float(any(cnt.get((r.summer,r.candidate_club,t),0)>0 for t in tw))
main['twin_any'] = [twin_any(r) for r in main.itertuples()]
ev_has = main.groupby('event_id').twin_any.apply(lambda x: x.notna().all())
tw = main[main.event_id.isin(ev_has.index[ev_has])].copy()
print('twin sample events:',tw.event_id.nunique(),' prevalence same/twin among alternatives:',tw.same.mean().round(3),tw.twin_any.mean().round(3))
ns['records'].clear()
fit('R2 twin sample: own agency only',tw,['same']+controls+flows,*D)
fit('R2 twin sample: own + twin agencies',tw,['same','twin_any']+controls+flows,*D,{'own agency':{'same':1},'twin agencies (placebo)':{'twin_any':1},'difference (own - twin)':{'same':1,'twin_any':-1}})
fit('R2 twin sample: twin only',tw,['twin_any']+controls+flows,*D,{'twin agencies (placebo)':{'twin_any':1}})
fit('R2 recent vs long-standing co-clients',main,['same_recent','same_old']+controls+flows,*D,
    {'recently placed co-client':{'same_recent':1},'long-standing co-client':{'same_old':1},
     'difference (recent - old)':{'same_recent':1,'same_old':-1}})
r = pd.DataFrame(ns['records']); r.to_csv('rev2_identification_results.csv',index=False)
pd.set_option('display.width',200)
print(r[~r.term.isin(['forward_3','reverse_3'])][['model','term','events','odds_ratio','label_lo','label_hi','label_p']].round(3).to_string())
ch=main[main.chosen==1]
print('chosen with recent co-client',ch.same_recent.sum(),' old only',ch.same_old.sum())

# ---- twin placebo within contract-status groups
import zipfile
from common import ROOT
K=ROOT/'raw'/'kaggle'
def rd(v):
    p=K/f'data_{v}'
    try: return pd.read_csv(zipfile.ZipFile(p).open('players.csv'),dtype=str,low_memory=False)
    except zipfile.BadZipFile: return pd.read_csv(p,dtype=str,low_memory=False)
ce=pd.concat([rd(v)[['player_id','contract_expiration_date']].assign(summer=s) for s,v in {2023:240,2024:435,2025:600}.items()])
ce['cexp']=pd.to_datetime(ce.contract_expiration_date.str[:10],errors='coerce')
tw=tw.merge(ce[['summer','player_id','cexp']],on=['summer','player_id'],how='left')
tw['expiring']=tw.cexp.notna()&(tw.cexp<=pd.to_datetime(tw.summer.astype(str)+'-06-30'))
tw['longer']=tw.cexp.notna()&(tw.cexp>pd.to_datetime(tw.summer.astype(str)+'-06-30')+pd.Timedelta(days=365))
ns['records'].clear()
for lab,sub in (('expiring contract',tw[tw.expiring]),('contract runs longer',tw[tw.longer])):
    fit(f'R2 twin placebo, {lab}',sub,['same','twin_any']+controls+flows,*D,{'own agency':{'same':1},'twin agencies (placebo)':{'twin_any':1},'difference (own - twin)':{'same':1,'twin_any':-1}})
r2=pd.DataFrame(ns['records']); r2.to_csv('rev2b_twin_by_contract_results.csv',index=False)
print(r2[r2.term.isin(['own agency','twin agencies (placebo)','difference (own - twin)'])][['model','term','events','odds_ratio','label_lo','label_hi']].round(3).to_string())

# ---- how close are the twins?  cosine similarity of the three profile blocks: twins vs all size-eligible agencies
rows=[]
for sm,d in pl.groupby('summer'):
    n=d.groupby('label').size()
    blocks={'nationality':pd.crosstab(d.label,d.nation.replace('','NA')),'position':pd.crosstab(d.label,d.pos),'age band':pd.crosstab(d.label,d.ageband)}
    B={k:v.div(v.sum(1),axis=0) for k,v in blocks.items()}
    for (s2,l),tws in twin_of.items():
        if s2!=sm: continue
        elig=[x for x in n.index if x!=l and n[x]>=max(4,0.5*n[l]) and n[x]<=2*n[l]]
        for k,F in B.items():
            def cos(a,b): return float(F.loc[a]@F.loc[b]/np.linalg.norm(F.loc[a])/np.linalg.norm(F.loc[b]))
            rows.append({'block':k,'twin':np.mean([cos(l,t) for t in tws]),'eligible_pool':np.mean([cos(l,x) for x in elig])})
mq=pd.DataFrame(rows).groupby('block').mean().round(3); mq.to_csv('rev2_matching_quality.csv'); print(mq)

# ---- appendix material: size ratio, coverage, anonymised worked example
srows=[]
for (sm,l),tws in twin_of.items():
    d=pl[pl.summer==sm]; n=d.groupby('label').size()
    srows.append({'summer':sm,'focal_clients':n[l],'mean_twin_clients':np.mean([n[x] for x in tws]),'min_ratio':min(n[x] for x in tws)/n[l],'max_ratio':max(n[x] for x in tws)/n[l]})
sz=pd.DataFrame(srows); 
elig_labels=pl.groupby('summer').label.apply(lambda s:(s.value_counts()>=3).sum())
cov=sz.groupby('summer').size().to_frame('focal labels with twins').join(elig_labels.rename('labels with at least 3 clients'))
cov['median twin/focal size']=sz.assign(r=sz.mean_twin_clients/sz.focal_clients).groupby('summer').r.median().round(2)
cov.to_csv('rev2_matching_coverage.csv'); print(cov)
# worked example: a focal label near the median size in 2024, anonymised
d=pl[pl.summer==2024]; n=d.groupby('label').size()
cands=[l for (s2,l) in twin_of if s2==2024 and 18<=n[l]<=26]
foc=sorted(cands,key=lambda l:abs(n[l]-n[cands].median()))[0]
def prof(l):
    x=d[d.label==l]; nat=x.nation.replace('','NA').value_counts(normalize=True).head(3); po=x.pos.value_counts(normalize=True); ag=x.ageband.value_counts(normalize=True)
    return dict(clients=len(x),top_nationalities=', '.join(f'{k} {v:.0%}' for k,v in nat.items()),forwards=f"{po.get('Attack',0):.0%}",midfield=f"{po.get('Midfield',0):.0%}",defenders=f"{po.get('Defender',0):.0%}",goalkeepers=f"{po.get('Goalkeeper',0):.0%}",age_21_or_under=f"{ag.get('0',0):.0%}",age_22_25=f"{ag.get('1',0):.0%}",age_26_29=f"{ag.get('2',0):.0%}",age_30_plus=f"{ag.get('3',0):.0%}")
ex=[dict(agency='Focal agency',**prof(foc))]+[dict(agency=f'Twin {k+1}',**prof(t)) for k,t in enumerate(twin_of[(2024,foc)])]
pd.DataFrame(ex).to_csv('rev2_worked_example.csv',index=False); print(pd.DataFrame(ex).to_string())
print(sorted(d.pos.unique()))
