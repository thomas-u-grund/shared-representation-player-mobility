"""Contract expiry (out-of-contract movers), label harmonisation, no-FE check, chance baseline."""
import warnings; warnings.filterwarnings('ignore')
import sys, re, zipfile, unicodedata; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
ns = load(); ev, tr = enrich(ns)
c, fit, players = ns['c'], ns['fit'], ns['players']
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
flows=['forward_3','reverse_3','own_history_3']; D=('candidate_club','candidate_league')
m = add_event_flags(c[~c.likely_return], ev)
m = m[m.snapshot_club.ne(m.destination_club)].copy()   # drop loan-to-buy conversions
K=ROOT/'raw'/'kaggle'
def rd(v):
    p=K/f'data_{v}'
    try: return pd.read_csv(zipfile.ZipFile(p).open('players.csv'),dtype=str,low_memory=False)
    except zipfile.BadZipFile: return pd.read_csv(p,dtype=str,low_memory=False)
ce = pd.concat([rd(v)[['player_id','contract_expiration_date']].assign(summer=s) for s,v in {2023:240,2024:435,2025:600}.items()])
ce['cexp']=pd.to_datetime(ce.contract_expiration_date.str[:10],errors='coerce')
m = m.merge(ce[['summer','player_id','cexp']],on=['summer','player_id'],how='left')
win = pd.to_datetime(m.summer.astype(str)+'-06-30')
m['exp_status'] = np.select([m.cexp.isna(), m.cexp<=win, m.cexp<=win+pd.Timedelta(days=365)],['unknown','expiring','one more year'],'longer')
ch = m[m.chosen.eq(1)]
print(pd.crosstab(ch.exp_status,[ch.turn],margins=True))
print(pd.crosstab(ch.exp_status,ch.paid,margins=True))
ns['records'].clear()
for k in ('expiring','one more year','longer','unknown'):
    m['same_'+k.replace(' ','_')]=m.same*m.exp_status.eq(k)
t4={'contract expiring by 30 June':{'same_expiring':1},'contract runs one more year':{'same_one_more_year':1},'contract runs longer':{'same_longer':1},'contract end unknown':{'same_unknown':1}}
fit('R6 by contract status (main sample)',m,['same_expiring','same_one_more_year','same_longer','same_unknown']+controls+flows,*D,t4)
fit('R6 expiring vs longer contract (main sample)',m,['same_expiring','same_one_more_year','same_longer','same_unknown']+controls+flows,*D,{'difference, expiring vs longer contract (ratio)':{'same_expiring':1,'same_longer':-1},'difference, expiring vs one more year (ratio)':{'same_expiring':1,'same_one_more_year':-1}})
fit('R6 expiring only (main sample)',m[m.exp_status.eq('expiring')],['same']+controls+flows,*D)
fit('R6 not expiring (main sample)',m[m.exp_status.isin(['one more year','longer'])],['same']+controls+flows,*D)
fit('R6 expiring, on turn dates only',m[m.exp_status.eq('expiring')&m.turn],['same']+controls+flows,*D)
fit('R6 expiring, off turn dates',m[m.exp_status.eq('expiring')&~m.turn],['same']+controls+flows,*D)
# no fixed effects, with flows (does pruning of never-chosen clubs matter?)
fit('R6 no club FE, with flows',m,['same']+controls+flows)

o = m[m.link_type.eq('origin')]   # events linked as in the first version
# chance baseline for effect size, both samples
for tag,x in (('first-version linkage events',o),('main sample',m)):
    g = x.groupby('event_id').agg(k=('same','sum'),n=('same','size')); ex = g[g.k>0]
    chosen = x[x.chosen.eq(1)].set_index('event_id').same
    print(tag,'| events with a connected alternative',len(ex),'chosen connected',int(chosen.reindex(ex.index).sum()),
          'observed share',round(chosen.reindex(ex.index).mean(),3),'expected under uniform choice',round((ex.k/ex.n).mean(),3))
r=pd.DataFrame(ns['records']); r.to_csv('rev6_misc_results.csv',index=False)
pd.set_option('display.width',200)
print(r[~r.term.isin(['forward_3','reverse_3'])][['model','term','events','odds_ratio','label_lo','label_hi','label_p']].round(3).to_string())
