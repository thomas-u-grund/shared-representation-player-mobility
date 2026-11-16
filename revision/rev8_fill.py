"""Recover missing representative labels from the archived releases and rebuild the linkage.

variants (cumulative):
  base      baseline release only                         (the extended linkage already reported)
  pre       + latest earlier PRE-window release label for the same player (no look-ahead),
            + movers listed at clubs outside the 14 leagues
  post      + earliest label in a release up to ~15 months AFTER the window (look-ahead; sensitivity only)
Output (written to ../coach_retry): perf_extended (variant pre = MAIN), perf_extended_base, perf_extended_post, each with fill_audit.csv
"""
import warnings; warnings.filterwarnings('ignore')
import argparse, csv, shutil, subprocess, sys, tempfile, zipfile
from pathlib import Path
import numpy as np, pandas as pd
from common import ROOT, CR
here=Path(__file__).resolve().parent; K=ROOT/'raw'/'kaggle'
ap=argparse.ArgumentParser(); ap.add_argument('variant',nargs='?',default='pre',choices=('base','pre','post'))
ap.add_argument('--expanded',type=Path,default=CR/'expanded',help='folder with player_snapshots.csv, summer_transfers.csv, destination_clubs.csv (and games.gz)')
ap.add_argument('--out',type=Path,default=None,help='output folder (default ../coach_retry/perf_extended[_base|_post])')
args=ap.parse_args(); variant=args.variant; EXP=args.expanded
dirname={'pre':'perf_extended','base':'perf_extended_base','post':'perf_extended_post'}[variant]
def rd(v):
    p=K/f'data_{v}'
    try: return pd.read_csv(zipfile.ZipFile(p).open('players.csv'),dtype=str,low_memory=False)
    except zipfile.BadZipFile: return pd.read_csv(p,dtype=str,low_memory=False)
vs=[240,325,355,435,547,566,600,603,606,660]
dates={240:'2023-05-23',325:'2023-12-29',355:'2024-02-09',435:'2024-05-31',547:'2024-12-27',566:'2025-02-11',600:'2025-04-11',603:'2026-02-14',606:'2026-02-14',660:'2026-05-26'}
rel={v:rd(v).drop_duplicates('player_id').set_index('player_id') for v in vs}
base_v={2023:240,2024:435,2025:600}
bad={'','null','none','-','nan','n/a','unknown','without agent'}
def clean(x): return x.strip() if isinstance(x,str) and x.strip().lower() not in bad else ''
def fill_label(s,pid):
    """return (label, source_version) from outside the baseline release, or ('','')"""
    win=f'{s}-06-01'
    if variant in ('pre','post'):
        for v in sorted([v for v in vs if dates[v]<win and v!=base_v[s]],reverse=True):   # latest earlier first
            if pid in rel[v].index and clean(rel[v].agent_name.get(pid)): return clean(rel[v].agent_name[pid]),v
    if variant=='post':
        lim=f'{s+1}-09-01'
        for v in sorted([v for v in vs if win<=dates[v]<lim]):                          # earliest later first
            if pid in rel[v].index and clean(rel[v].agent_name.get(pid)): return clean(rel[v].agent_name[pid]),v
    return '',''
st=Path(tempfile.mkdtemp(prefix='stage_')); out=(args.out or CR/dirname).resolve(); out.mkdir(parents=True,exist_ok=True)
for v in (240,435,600): shutil.copy2(K/f'data_{v}',st/f'{v}.data')
shutil.copy2(CR/'games.gz',st/'games.gz'); shutil.copy2(EXP/'destination_clubs.csv',st/'destination_clubs.csv')
snap=list(csv.DictReader(open(EXP/'player_snapshots.csv',encoding='utf-8-sig')))
fields=list(snap[0]); audit=[]; have=set()
for r in snap:
    if r['summer'] not in ('2023','2024','2025'): continue
    s=int(r['summer']); have.add((r['summer'],r['player_id']))
    if not clean(r['agent_label']):
        lab,src=fill_label(s,r['player_id'])
        if lab:
            r['agent_label']=lab; audit.append(dict(summer=s,player_id=r['player_id'],source=src,kind='roster_fill'))
rows=list(csv.DictReader(open(EXP/'summer_transfers.csv')))
# movers listed at clubs outside the 14 leagues (variant pre/post): add a snapshot row so the mover's attributes and label are available
if variant in ('pre','post'):
    for e in rows:
        s=int(e['summer']); key=(e['summer'],e['player_id'])
        if key in have: continue
        v=base_v[s]; pid=e['player_id']
        if pid in rel[v].index:
            lab=clean(rel[v].agent_name.get(pid)) or fill_label(s,pid)[0]
            rrow=rel[v].loc[pid]
            snap.append(dict(summer=e['summer'],version=str(v),league=str(rrow.get('current_club_domestic_competition_id','')),player_id=pid,
                             club_id=str(rrow.get('current_club_id','')),player=str(rrow.get('name','')),agent_label=lab))
            have.add(key)
            if lab: audit.append(dict(summer=s,player_id=pid,source=v,kind='outside_14_leagues'))
with open(st/'player_snapshots.csv','w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=fields); w.writeheader(); w.writerows(snap)
lab_by={(r['summer'],r['player_id']):bool(clean(r['agent_label'])) for r in snap if r['summer'] in ('2023','2024','2025')}
club_by={(r['summer'],r['player_id'],r['club_id']) for r in snap}
for e in rows:
    k=(e['summer'],e['player_id'])
    at_origin=(e['summer'],e['player_id'],e['origin_club']) in club_by
    known=lab_by.get(k,False)
    e['baseline_match']=str(at_origin or k in lab_by); e['known_agent']=str(known)
    e['link_type']='origin' if (at_origin and known) else ('player_only' if known else '')
with open(st/'summer_transfers.csv','w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
pd.DataFrame(audit).to_csv(out/'fill_audit.csv',index=False)
print(variant,'filled rows:',pd.DataFrame(audit).groupby('kind').size().to_dict() if audit else {})
subprocess.run([sys.executable,str(here/'build_performance_relaxed.py'),str(st),str(ROOT/'raw/performance'),str(out)],check=True,cwd=out,stdout=subprocess.DEVNULL)
t=pd.read_csv(out/'transfer_performance.csv'); print(variant,'events',len(t),'agent-linked',int((t.known_agent==True).sum()),'link types',t.link_type.value_counts().to_dict())
shutil.rmtree(st)
