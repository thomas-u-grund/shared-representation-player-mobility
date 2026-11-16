"""Rerun the two paper analyses and check them against the archived estimates."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

import numpy as np
import pandas as pd

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('reproduced'))
args=parser.parse_args()
root=Path(__file__).resolve().parent
out=args.output.resolve()
assert out != root/'agency_paper', 'Choose a separate output directory'
out.mkdir(parents=True,exist_ok=True)
env=dict(os.environ,OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
paper=root/'agency_paper'
# Main (extended) sample and the first-version (archived) sample
runs=[('extended','',''),('archived','_archived','archived_')]
for sample,suffix,prefix in runs:
    sub=out/sample; sub.mkdir(exist_ok=True)
    for script,paths in [('analyse.py',[root/'coach_retry']),('compare_windows.py',[root,root/'raw/kaggle'])]:
        with (sub/(script.removesuffix('.py')+'.log')).open('w') as log:
            subprocess.run([sys.executable,str(paper/script),*map(str,paths),sample],cwd=sub,env=env,
                           stdout=log,stderr=subprocess.STDOUT,check=True)
    for name in ('model_results','season_comparison_results'):
        expected=pd.read_csv(paper/f'{name}{suffix}.csv')
        realised=pd.read_csv(sub/f'{name}.csv')
        pd.testing.assert_frame_equal(expected,realised,check_exact=False,rtol=0.002,atol=0.0001)
        print(f'{sample}: {name}: {len(realised)} rows, agrees with archived results')
print(f'Outputs: {out}')
