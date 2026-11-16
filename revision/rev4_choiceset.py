"""Point 4: plausible choice sets.  Tier/affordability fit between mover and candidate club as controls,
and as outcome-independent restrictions of the alternatives."""
import warnings; warnings.filterwarnings('ignore')
import sys; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
ns = load(); ev, tr = enrich(ns)
c, fit, players = ns['c'], ns['fit'], ns['players']
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
flows=['forward_3','reverse_3','own_history_3']; D=('candidate_club','candidate_league')
m = add_event_flags(c[~c.likely_return], ev)
m = m[m.snapshot_club.ne(m.destination_club)].copy()   # drop loan-to-buy conversions
pl = players.copy(); pl['mv']=pd.to_numeric(pl.market_value_eur,errors='coerce'); pl['age']=pd.to_numeric(pl.age_at_cutoff,errors='coerce')
club = pl.groupby(['summer','club_id']).agg(club_med_mv=('mv','median'),club_med_age=('age','median')).reset_index()
m = m.merge(club,left_on=['summer','candidate_club'],right_on=['summer','club_id'],how='left')
m['mv_mover']=pd.to_numeric(m.market_value_eur,errors='coerce')
m['value_gap'] = np.log(m.mv_mover.fillna(np.nan)+1e5)-np.log(m.club_med_mv+1e5)
m['value_gap_unknown']=m.value_gap.isna().astype(int)
m['abs_value_gap']=m.value_gap.abs().fillna(0); m['sq_value_gap']=(m.value_gap**2).fillna(0)
m['signed_value_gap']=m.value_gap.fillna(0)
m['abs_age_gap']=(m.player_age-m.club_med_age).abs().fillna(0)/5
m['abs_elo_gap']=((m.candidate_club_elo-m.origin_elo).abs()/100).fillna(0); m['elo_gap_unknown']=(m.candidate_club_elo.isna()|m.origin_elo.isna()).astype(int)
fitc=['abs_value_gap','sq_value_gap','signed_value_gap','value_gap_unknown','abs_age_gap','abs_elo_gap','elo_gap_unknown']
print('value gap among alternatives vs chosen:',m.groupby('chosen').abs_value_gap.mean().round(2).to_dict())
ns['records'].clear()
fit('R4 main sample',m,['same']+controls+flows,*D)
fit('R4 + tier/affordability fit controls',m,['same']+controls+flows+fitc,*D)
# outcome-independent-ish restriction: alternatives within a factor of ~4.5 in squad value (|gap|<=1.5) or unknown
for w in (1.5,1.0):
    keep = m[(m.value_gap.abs()<=w)|m.value_gap.isna()]
    # events whose chosen club falls outside are dropped by valid(); report how many
    n0=m.event_id.nunique()
    fit(f'R4 alternatives within |value gap|<={w}',keep,['same']+controls+flows+fitc,*D)
# restrict to clubs whose Elo is within 200 points of the origin club (where known)
ek = m[(m.abs_elo_gap<=2)|m.elo_gap_unknown.eq(1)]
fit('R4 alternatives within 200 Elo points of origin club',ek,['same']+controls+flows+fitc,*D)
# same-tier only: candidate clubs in the same ppg tercile as the destination? (not outcome independent) -> skipped on purpose
r=pd.DataFrame(ns['records']); r.to_csv('rev4_choiceset_results.csv',index=False)
pd.set_option('display.width',200)
print(r[r.term.isin(['same'])][['model','events','alternatives','odds_ratio','label_lo','label_hi','label_p']].round(3).to_string())
