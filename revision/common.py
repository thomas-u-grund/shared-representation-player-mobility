"""Shared set-up for the revision analyses.

Executes the data preparation and the `fit` estimator of ../agency_paper/analyse.py unchanged (everything before
the first model call) for the chosen sample, then adds event-level enrichment.
  sample='extended'  main sample (coach_retry/perf_extended)
  sample='archived'  first-version sample (coach_retry/perf)
"""
import sys
from pathlib import Path
import numpy as np, pandas as pd

ROOT = Path(__file__).resolve().parents[1]
CR = ROOT/'coach_retry'

def load(sample='extended'):
    src = (ROOT/'agency_paper'/'analyse.py').read_text().split("main=c[~c.likely_return]")[0]
    ns = {'__name__':'analyse_prefix'}
    sys.argv = ['analyse.py', str(CR), sample]
    exec(compile(src,'analyse_prefix','exec'), ns)
    ns['sample'] = sample
    return ns

def enrich(ns, sample='extended'):
    """Add fee, former-club flags at event level (merged onto the candidate frame by event_id)."""
    tr = pd.read_csv(CR/'transfers.gz', dtype=str)
    tr['fee'] = pd.to_numeric(tr.transfer_fee)
    pp = CR/('perf' if sample=='archived' else f'perf_{sample}')
    tp = pd.read_csv(pp/'transfer_performance.csv', dtype={'player_id':str,'origin_club':str,'destination_club':str})
    g = tr.groupby(['player_id','transfer_date','from_club_id','to_club_id'],as_index=False).fee.max()
    tp = tp.merge(g, left_on=['player_id','date','origin_club','destination_club'],
                  right_on=['player_id','transfer_date','from_club_id','to_club_id'], how='left')
    hist = tr[['player_id','transfer_date','from_club_id','to_club_id']].drop_duplicates()
    spells = hist.groupby('player_id').apply(lambda d: list(zip(d.transfer_date,d.from_club_id,d.to_club_id)), include_groups=False).to_dict()
    def former(r):
        return any(d < r.date and (a == r.destination_club or b == r.destination_club) for d,a,b in spells.get(r.player_id,()))
    tp['former_dest'] = tp.apply(former, axis=1)
    tp['paid'] = tp.fee.fillna(0).gt(0)
    return tp[['event_id','fee','former_dest','paid']], tr

def add_event_flags(frame, ev):
    f = frame.merge(ev, on='event_id', how='left', validate='many_to_one')
    f['turn'] = f.date.str[-5:].isin(['06-30','07-01'])
    return f
