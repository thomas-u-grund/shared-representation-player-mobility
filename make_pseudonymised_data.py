"""Create the pseudonymised summer analysis inputs in data_pseudonymised/ (used by reproduce_summer.py).

Run from the project root that holds the raw-derived files (coach_retry/perf_extended, coach_retry/transfers.gz).
Player identifiers and representative labels are replaced by random identifiers drawn with a fresh random permutation;
the mapping is not stored, so the files cannot be linked back to named individuals from this repository.
Player names, EA identifiers and all name columns are dropped. Everything the estimators read is preserved.
"""
import re, secrets, sys, unicodedata
from pathlib import Path
import numpy as np, pandas as pd
src=Path(sys.argv[1]); out=Path(sys.argv[2]); (out/'perf_extended').mkdir(parents=True,exist_ok=True)
rng=np.random.default_rng(int.from_bytes(secrets.token_bytes(16),'big'))
P=src/'perf_extended'
pp=pd.read_csv(P/'player_performance.csv',dtype=str,keep_default_na=False)
tp=pd.read_csv(P/'transfer_performance.csv',dtype=str,keep_default_na=False)
wc=pd.read_csv(P/'within_league_candidates.csv',dtype=str,keep_default_na=False)
hist=pd.read_csv(src/'transfers.gz',usecols=['player_id','transfer_date','from_club_id','to_club_id','transfer_fee'],dtype=str,keep_default_na=False)
def key(s):
    return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKD',str(s).casefold()).encode('ascii','ignore').decode())
def mapping(values,fmt):
    u=np.array(sorted(set(values))); perm=rng.permutation(len(u)); return {v:fmt.format(i+1) for v,i in zip(u,perm)}
pid=mapping(list(pp.player_id)+list(tp.player_id)+list(hist.player_id),'{}')
lab=mapping([key(l) for l in pp.agent_label if key(l)],'rep{:05d}')   # labels that normalise identically stay identical
for f in (pp,tp,hist): f['player_id']=f.player_id.map(pid)
pp['agent_label']=pp.agent_label.map(lambda l: lab.get(key(l),''))
pp=pp.drop(columns=['player','ea_id'])
pp.to_csv(out/'perf_extended/player_performance.csv',index=False)
tp.to_csv(out/'perf_extended/transfer_performance.csv',index=False)
wc.to_csv(out/'perf_extended/within_league_candidates.csv',index=False)
hist.to_csv(out/'transfers.gz',index=False,compression='gzip')
print({k:len(v) for k,v in dict(players=pp,events=tp,candidates=wc,history=hist).items()},'representative labels:',len(lab))
