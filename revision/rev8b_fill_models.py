"""Sample variants: first-version linkage, player-level linkage without / with carry-forward labels (main), and with
look-ahead labels from later releases (sensitivity only).  Plus label harmonisation on the main sample."""
import warnings; warnings.filterwarnings('ignore')
import sys, re, unicodedata; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
flows=['forward_3','reverse_3','own_history_3']; D=('candidate_club','candidate_league')
recs=[]
names={'archived':'first-version linkage','extended_base':'player-level linkage, baseline labels','extended':'MAIN: + carry-forward labels','extended_post':'+ look-ahead labels (later releases)'}
for smp in ('archived','extended_base','extended','extended_post'):
    ns=load(smp); ev,tr=enrich(ns,smp); c,fit=ns['c'],ns['fit']
    m=add_event_flags(c[~c.likely_return],ev); ns['records'].clear()
    n=names[smp]
    fit(f'V {n}',m,['same']+controls+flows,*D)
    fit(f'V {n} | omit 30 June and 1 July',m[~m.turn],['same']+controls+flows,*D)
    fit(f'V {n} | club-by-summer FE',m,['same']+controls+flows,'club_summer','candidate_league')
    if smp=='extended':
        stop=set('gmbh ltd limited llc sl srl spa sa bv ag oy ab as inc co company group agency agencia agentur management managment sports sport football futbol soccer consulting consultancy international intl world the of and de la el'.split())
        def hkey(s,orig):
            s2=unicodedata.normalize('NFKD',str(s).casefold()).encode('ascii','ignore').decode()
            toks=[t for t in re.split(r'[^a-z0-9]+',s2) if t and t not in stop]
            return ''.join(sorted(set(toks))) or orig
        pl=ns['players']; pl=pl[pl.label.ne('')].copy(); pl['hlabel']=[hkey(a,o) for a,o in zip(pl.agent_label,pl.label)]
        lab_h=pl.drop_duplicates('label').set_index('label').hlabel
        mm=m.copy(); mm['hlabel']=mm.label.map(lab_h)
        cnt=pl.groupby(['summer','club_id','hlabel']).size()
        k=cnt.reindex(pd.MultiIndex.from_frame(mm[['summer','candidate_club','hlabel']])).fillna(0).to_numpy()-mm.candidate_club.eq(mm.snapshot_club).to_numpy()
        mm['same']=(k>0).astype(int)
        print('harmonised: connected share',m.same.mean().round(4),'->',mm.same.mean().round(4))
        fit('V MAIN | harmonised labels',mm,['same']+controls+flows,*D)
        fit('V MAIN | no club fixed effects',m,['same']+controls+flows)
    recs+=ns['records']
r=pd.DataFrame(recs); r.to_csv('rev8_fill_results.csv',index=False)
pd.set_option('display.width',220)
print(r[r.term=='same'][['model','events','odds_ratio','label_lo','label_hi']].round(3).to_string())
