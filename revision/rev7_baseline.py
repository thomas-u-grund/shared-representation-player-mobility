import warnings; warnings.filterwarnings('ignore')
import sys; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
def sample_of(ns,x,valid):
    x=valid(x)
    while True:
        n=len(x); x=valid(x[x.candidate_club.isin(x.loc[x.chosen.eq(1),'candidate_club'])])
        if n==len(x): return x
rows=[]
for tag,smp in (('first-version sample','archived'),('main sample','extended')):
    ns=load(smp); ev,tr=enrich(ns,smp); c,valid=ns['c'],ns['valid']
    m=add_event_flags(c[~c.likely_return],ev)
    s=sample_of(ns,m,valid); g=s.groupby('event_id').agg(k=('same','sum'),n=('same','size')); ex=g[g.k>0]
    ch=s[s.chosen.eq(1)].set_index('event_id').same.reindex(ex.index)
    rows.append(dict(sample=tag,events=len(g),exposed_events=len(ex),chosen_connected=int(ch.sum()),observed_share=ch.mean(),uniform_share=(ex.k/ex.n).mean()))
r=pd.DataFrame(rows); print(r.round(3).to_string()); r.to_csv('rev7_baseline.csv',index=False)
