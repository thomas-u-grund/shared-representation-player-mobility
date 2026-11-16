"""Point 1: what drives the turn-of-July heterogeneity? Transaction type and returns to former clubs."""
import sys; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
ns = load(); ev, tr = enrich(ns)
c, fit, controls = ns['c'], ns['fit'], None
controls=['log_known','nationality_share_10pp','nationality_unknown','ppg_rank','ppg_missing','elo100','elo_missing','position_count','position_minutes','position_unknown','position_minutes_unknown','rating_gap','rating_gap_unknown']
flows=['forward_3','reverse_3','own_history_3']
m = add_event_flags(c[~c.likely_return], ev)
m['same_former'] = m.same*m.former_dest
m['same_paid'] = m.same*m.paid
m['same_turn_free'] = m.same*(m.turn & ~m.paid)
m['same_turn_paid'] = m.same*(m.turn & m.paid)
m['same_off_free'] = m.same*(~m.turn & ~m.paid)
m['same_off_paid'] = m.same*(~m.turn & m.paid)
ch = m[m.chosen.eq(1)]
print('events',len(ch)); print(pd.crosstab([ch.turn,ch.paid],ch.former_dest,margins=True))
ns['records'].clear()
D=('candidate_club','candidate_league')
fit('R1 baseline',m,['same']+controls+flows,*D)
fit('R1 same x former-club destination',m,['same','same_former']+controls+flows,*D,{'shared, not former club':{'same':1},'shared, former club':{'same':1,'same_former':1}})
m2 = m[~m.former_dest.fillna(False)]
fit('R1 exclude former-club destinations',m2,['same']+controls+flows,*D)
fit('R1 exclude former, turn x same',m2,['same','same_turn_of_july']+controls+flows,*D,{'off-turn':{'same':1},'turn':{'same':1,'same_turn_of_july':1}})
fit('R1 four-way transaction type',m,['same_turn_free','same_turn_paid','same_off_free','same_off_paid']+controls+flows,*D,{'turn, free/loan':{'same_turn_free':1},'turn, paid':{'same_turn_paid':1},'off-turn, free/loan':{'same_off_free':1},'off-turn, paid':{'same_off_paid':1}})
fit('R1 four-way, exclude former',m2,['same_turn_free','same_turn_paid','same_off_free','same_off_paid']+controls+flows,*D,{'turn, free/loan':{'same_turn_free':1},'turn, paid':{'same_turn_paid':1},'off-turn, free/loan':{'same_off_free':1},'off-turn, paid':{'same_off_paid':1}})
fit('R1 paid only',m[m.paid.fillna(False)],['same']+controls+flows,*D)
fit('R1 no fee and not former club',m2[~m2.paid.fillna(False)],['same']+controls+flows,*D)
# stricter return screen: also drop zero-fee moves on turn dates to a former club
strict = m[~(m.former_dest.fillna(False) & m.turn & ~m.paid.fillna(False))]
fit('R1 strict return screen',strict,['same']+controls+flows,*D)
fit('R1 strict screen, turn x same',strict,['same','same_turn_of_july']+controls+flows,*D,{'off-turn':{'same':1},'turn':{'same':1,'same_turn_of_july':1}})
r = pd.DataFrame(ns['records'])
r.to_csv('rev1_timing_results.csv',index=False)
pd.set_option('display.width',200)
print(r[['model','term','events','odds_ratio','label_lo','label_hi','label_p']].round(3).to_string())
