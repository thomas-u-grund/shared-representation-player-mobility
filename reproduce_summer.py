"""Re-estimate the summer models on the pseudonymised data and compare with the stored results.

Needs only data_pseudonymised/ (see README). Writes the new estimates to --output and checks them against
agency_paper/model_results.csv (42 rows: the club and league models for the main sample).
"""
import argparse, os, subprocess, sys
from pathlib import Path
import pandas as pd
ap=argparse.ArgumentParser(); ap.add_argument('--output',type=Path,default=Path('reproduced_summer')); args=ap.parse_args()
root=Path(__file__).resolve().parent; out=args.output.resolve(); out.mkdir(parents=True,exist_ok=True)
data=root/'data_pseudonymised'
assert (data/'perf_extended'/'player_performance.csv').exists(), 'data_pseudonymised/ is missing; see README.md'
env=dict(os.environ,OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
with (out/'analyse.log').open('w') as log:
    subprocess.run([sys.executable,str(root/'agency_paper'/'analyse.py'),str(data),'extended'],cwd=out,env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
expected=pd.read_csv(root/'agency_paper'/'model_results.csv'); realised=pd.read_csv(out/'model_results.csv')
pd.testing.assert_frame_equal(expected,realised,check_exact=False,rtol=0.002,atol=0.0001)
print(f'model_results: {len(realised)} rows agree with the stored estimates (rtol 0.2%). Outputs in {out}')
