"""Rebuild the summer and winter tables from bundled archived inputs."""
import argparse
import csv
import os
from pathlib import Path
import shutil
import subprocess
import sys

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('rebuilt_data'))
args=parser.parse_args()
root=Path(__file__).resolve().parent
out=args.output.resolve()
out.mkdir(parents=True,exist_ok=False)
env=dict(os.environ,OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
kaggle=root/'raw/kaggle'
coach=root/'coach_retry'
def run(script,work,*inputs):
    print('Running',script.name,flush=True)
    subprocess.run([sys.executable,str(script),*map(str,inputs)],cwd=work,env=env,check=True)
def stage(a,b):
    try:
        os.link(a,b)
    except OSError:
        shutil.copy2(a,b)

expanded=out/'expanded'
perf=out/'perf'
observed=out/'winter_recovery'
proxy=out/'winter_proxy'
alternate=proxy/'version_606_sensitivity'
for d in (expanded,perf,observed,proxy,alternate):
    d.mkdir(parents=True,exist_ok=True)
for v in (240,435,600,660):
    stage(kaggle/f'data_{v}',expanded/f'{v}.data')
for name in ('games.gz','transfers.gz'):
    stage(coach/name,expanded/name)
run(coach/'expanded/count.py',expanded)
# count.py deduplicates into a set. Preserve the published event order
# before build_performance.py assigns numbered event IDs.
event_columns=('date','player_id','origin_club','destination_club')
def event_key(row):
    return tuple(row[k] for k in event_columns)
generated=expanded/'summer_transfers.csv'
with generated.open(newline='') as f:
    reader=csv.DictReader(f)
    fields=reader.fieldnames
    events={event_key(row):row for row in reader}
with (coach/'expanded/summer_transfers.csv').open(newline='') as f:
    order=[event_key(row) for row in csv.DictReader(f)]
if len(order)!=len(events) or set(order)!=events.keys():
    raise ValueError('Rebuilt summer event keys differ from the archived panel')
with generated.open('w',newline='') as f:
    writer=csv.DictWriter(f,fieldnames=fields)
    writer.writeheader()
    writer.writerows(events[k] for k in order)
run(coach/'expanded/fit.py',expanded)
run(coach/'perf/build_performance.py',perf,expanded,root/'raw/performance',perf)
shutil.copy2(root/'winter_recovery/sources.csv',observed)
run(root/'winter_recovery/build_winter.py',observed,kaggle,coach)
for old,new in [(root/'winter_proxy',proxy),(root/'winter_proxy/version_606_sensitivity',alternate)]:
    shutil.copy2(old/'proxy_sources.csv',new)
    run(root/'winter_proxy/evaluate.py',new,kaggle,coach,observed)
# Extended sample (main): player-level linkage, carry-forward labels, outside-league movers; see revision/README.md
run(root/'revision/rev8_fill.py',root/'revision','pre','--expanded',expanded,'--out',out/'perf_extended')
observed_ext=out/'winter_recovery_extended'
observed_ext.mkdir(parents=True,exist_ok=True)
shutil.copy2(root/'winter_recovery/sources.csv',observed_ext)
run(root/'winter_recovery/build_winter_extended.py',observed_ext,kaggle,coach)
print('Rebuilt data:',out)
