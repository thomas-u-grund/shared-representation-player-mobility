"""Create the pseudonymised analysis inputs in data_pseudonymised/ (read by the R and Python scripts).

Run from a project folder that holds the raw-derived files:
    python make_pseudonymised_data.py <coach_retry dir> <raw/kaggle dir> <output dir>
Player identifiers and representative labels are replaced by random codes drawn with a fresh random permutation. The mapping is
not stored, so the files cannot be linked back to named individuals from this repository. Player names and EA identifiers are
dropped. The estimators read everything else unchanged. Two derived columns are added so that analyses that originally used
the raw label text still run: `hlabel` (labels that differ only by generic words such as group, agency or management share a code)
and contract end dates (contract_dates.csv).
"""
import re, secrets, sys, unicodedata, zipfile
from pathlib import Path
import numpy as np, pandas as pd
src=Path(sys.argv[1]); kag=Path(sys.argv[2]); out=Path(sys.argv[3])
rng=np.random.default_rng(int.from_bytes(secrets.token_bytes(16),'big'))
def key(s): return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKD',str(s).casefold()).encode('ascii','ignore').decode())
STOP=set('gmbh ltd limited llc sl srl spa sa bv ag oy ab as inc co company group agency agencia agentur management managment sports sport football futbol soccer consulting consultancy international intl world the of and de la el'.split())
def hkey(s,orig):
    s2=unicodedata.normalize('NFKD',str(s).casefold()).encode('ascii','ignore').decode()
    toks=[t for t in re.split(r'[^a-z0-9]+',s2) if t and t not in STOP]
    return ''.join(sorted(set(toks))) or orig
DIRS={'perf':'perf','perf_extended':'perf_extended','perf_extended_base':'perf_extended_base','perf_extended_post':'perf_extended_post'}
FILES=['player_performance','transfer_performance','within_league_candidates']
data={d:{f:pd.read_csv(src/d/f'{f}.csv',dtype=str,keep_default_na=False) for f in FILES} for d in DIRS}
hist=pd.read_csv(src/'transfers.gz',usecols=['player_id','transfer_date','from_club_id','to_club_id','transfer_fee'],dtype=str,keep_default_na=False)
def rd(v):
    p=kag/f'data_{v}'
    try: return pd.read_csv(zipfile.ZipFile(p).open('players.csv'),dtype=str,low_memory=False)
    except zipfile.BadZipFile: return pd.read_csv(p,dtype=str,low_memory=False)
ce=pd.concat([rd(v)[['player_id','contract_expiration_date']].assign(summer=str(s)) for s,v in {2023:240,2024:435,2025:600}.items()])
ce['contract_expiration_date']=ce.contract_expiration_date.fillna('').str[:10]
WIN={'winter_recovery_extended':{'winter_player_snapshots':{2024:325,2025:547},'january_transfers':None,'january_destination_candidates':None},
     'winter_proxy':{'backcast_rosters':{2026:603},'backcast_january_transfers':None,'backcast_candidates':None},
     'winter_proxy/version_606_sensitivity':{'backcast_rosters':{2026:606},'backcast_january_transfers':None,'backcast_candidates':None}}
wdata={d:{f:pd.read_csv(src.parent/d/f'{f}.csv',dtype=str,keep_default_na=False) for f in fs} for d,fs in WIN.items()}
allids=set(hist.player_id)|set(ce.player_id)
for d in wdata.values():
    for t in d.values():
        if 'player_id' in t.columns: allids|=set(t.player_id)
for d in data.values(): allids|=set(d['player_performance'].player_id)|set(d['transfer_performance'].player_id)
def mapping(values,fmt):
    u=np.array(sorted(set(values))); perm=rng.permutation(len(u)); return {v:fmt.format(i+1) for v,i in zip(u,perm)}
pid=mapping(allids,'{}')
rawlabels=[l for d in data.values() for l in d['player_performance'].agent_label]
winlabels=[l for d in wdata.values() for t in d.values() if 'agent_label' in t.columns for l in t.agent_label]
lab=mapping([key(l) for l in rawlabels+winlabels if key(l)],'rep{:05d}')
hl=mapping([hkey(l,key(l)) for l in rawlabels if key(l)],'hrep{:05d}')
for dn,d in data.items():
    pp=d['player_performance']; tp=d['transfer_performance']
    pp['player_id']=pp.player_id.map(pid); tp['player_id']=tp.player_id.map(pid)
    k=pp.agent_label.map(key); h=[hkey(l,kk) if kk else '' for l,kk in zip(pp.agent_label,k)]
    first=pd.Series(h).groupby(k.values).first()          # first row of each label defines its harmonised label (as in the original analysis)
    pp['hlabel']=[hl[first[kk]] if kk else '' for kk in k]
    pp['agent_label']=[lab.get(kk,'') for kk in k]
    pp=pp.drop(columns=[c for c in ('player','ea_id') if c in pp.columns])
    (out/dn).mkdir(parents=True,exist_ok=True)
    pp.to_csv(out/dn/'player_performance.csv',index=False); tp.to_csv(out/dn/'transfer_performance.csv',index=False)
    d['within_league_candidates'].to_csv(out/dn/'within_league_candidates.csv',index=False)
hist['player_id']=hist.player_id.map(pid); hist.to_csv(out/'transfers.gz',index=False,compression='gzip')
ce['player_id']=ce.player_id.map(pid); ce[['summer','player_id','contract_expiration_date']].to_csv(out/'contract_dates.csv',index=False)
nat={v:rd(v)[['player_id','country_of_citizenship']].drop_duplicates('player_id').set_index('player_id').country_of_citizenship for v in (325,547,603,606)}
for d,fs in WIN.items():
    (out/d).mkdir(parents=True,exist_ok=True)
    for f,vers in fs.items():
        t=wdata[d][f].copy(); raw=t.player_id.copy() if 'player_id' in t.columns else None
        if vers:   # attach nationality (needed by the covariates) before the identifiers are replaced
            t['nationality']=[nat[vers[int(j)]].get(pid,'') if int(j) in vers else '' for j,pid in zip(t.january,raw)]
            t['nationality']=t.nationality.fillna('')
        if raw is not None: t['player_id']=t.player_id.map(pid)
        for c in ('player_name',): 
            if c in t.columns: t=t.drop(columns=c)
        if 'agent_label' in t.columns: t['agent_label']=[lab.get(key(l),'') for l in t.agent_label]
        if 'agent_key' in t.columns: t['agent_key']=''
        t.to_csv(out/d/f'{f}.csv',index=False)
print('written to',out)
