"""Point 5: league model with (i) nationality x destination-league fixed effects, (ii) agency concentration
(share of the agency's clients in the league) instead of a raw count that scales with agency and league size."""
import warnings; warnings.filterwarnings('ignore')
import sys; sys.path.insert(0,'.')
import numpy as np, pandas as pd
from common import *
full = (ROOT/'agency_paper'/'analyse.py').read_text()
seg = full[full.index("countries=dict("):full.index("fit('League fixed routes'")]
out=[]
for tag, sample in (('first-version sample','archived'),('main sample','extended')):
    ns = load(sample); ns['records'].clear()
    exec(seg, ns)
    league, fit, players = ns['league'], ns['fit'], ns['players']
    # agency concentration
    tot = ns['counts'].groupby(['summer','label']).sum()
    league['agency_total'] = tot.reindex(pd.MultiIndex.from_frame(league[['summer','label']])).to_numpy()
    cnt = np.expm1(league.agency_log)
    league['agency_share'] = (cnt/league.agency_total.replace(0,np.nan)).fillna(0)*10   # per 10 pp of the agency's clients
    # nationality x destination-league dummies (nationalities with >= 25 movers kept, others pooled as reference)
    nat_n = league[league.chosen.eq(1)].nation.value_counts(); keep = list(nat_n[nat_n>=25].index)
    cols=[]
    chosen_cells = league[league.chosen.eq(1)].groupby(['nation','alternative']).size()
    for n in keep:
        for a in ns['countries']:
            if chosen_cells.get((n,a),0) < 5: continue   # cells rarely or never chosen would diverge; pooled with reference
            nm=f'nl_{n}_{a}'; league[nm]=(league.nation.eq(n)&league.alternative.eq(a)).astype(float); cols.append(nm)
    base=['log_roster']
    def lf(name, x, terms, metrics=None):
        fit(f'{tag}: {name}', x, terms, 'route_fe','origin_league', metrics)
    lf('baseline (agency log count, local nationality)', league, ['agency_log','log_roster','local_nationality'])
    lf('+ nationality x destination-league FE', league, ['agency_log','log_roster']+cols, {'agency_log':{'agency_log':1}})
    lf('agency share, local nationality', league, ['agency_share','log_roster','local_nationality'], {'agency_share':{'agency_share':1}})
    lf('agency share + nationality x league FE', league, ['agency_share','log_roster']+cols, {'agency_share':{'agency_share':1}})
    lc = league[league.route.eq('between') & league.origin_league.ne(league.alternative)]
    lf('between-league, + nationality x league FE', lc, ['agency_log','log_roster']+cols, {'agency_log':{'agency_log':1}})
    out += ns['records']
r=pd.DataFrame(out); r.to_csv('rev5_league_results.csv',index=False)
pd.set_option('display.width',220)
r['or_per_doubling']=np.where(r.term=='agency_log',r.odds_ratio**np.log(2),np.nan)
r['lo_doubling']=np.where(r.term=='agency_log',r.label_lo**np.log(2),np.nan); r['hi_doubling']=np.where(r.term=='agency_log',r.label_hi**np.log(2),np.nan)
r.to_csv('rev5_league_results.csv',index=False)
print(r[r.term.isin(['agency_log','agency_share'])][['model','events','odds_ratio','label_lo','label_hi','or_per_doubling','lo_doubling','hi_doubling']].round(3).to_string())
