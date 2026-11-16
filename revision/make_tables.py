"""Collect the revision estimates into agency_paper/revision_results.csv and draw Figure 1 (main sample)."""
import pandas as pd, numpy as np
from pathlib import Path
import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
here=Path(__file__).resolve().parent; out=here.parent/'agency_paper'
def get(f,model,term,cl='label'):
    d=pd.read_csv(here/f); r=d[(d.model==model)&(d.term==term)]
    assert len(r)==1,(f,model,term,len(r)); r=r.iloc[0]
    return int(r.events),r.odds_ratio,r[f'{cl}_lo'],r[f'{cl}_hi']
rows=[]
def add(block,label,*a,src=''): rows.append(dict(block=block,label=label,events=a[0],odds_ratio=a[1],lo=a[2],hi=a[3],source=src))
f='rev6_misc_results.csv'; M='R6 by contract status (main sample)'
add('mechanism','Contract ends by 30 June of the window year',*get(f,M,'contract expiring by 30 June'),src=f)
add('mechanism','Contract runs one more year',*get(f,M,'contract runs one more year'),src=f)
add('mechanism','Contract runs longer',*get(f,M,'contract runs longer'),src=f)
add('mechanism','Ratio: expiring contract to longer contract',*get(f,'R6 expiring vs longer contract (main sample)','difference, expiring vs longer contract (ratio)'),src=f)
f='rev1_timing_results.csv'; M='R1 four-way transaction type'
for t,l in [('turn, free/loan','30 June or 1 July, no fee'),('turn, paid','30 June or 1 July, fee paid'),('off-turn, free/loan','Other dates, no fee'),('off-turn, paid','Other dates, fee paid')]:
    add('mechanism',l,*get(f,M,t),src=f)
add('mechanism','Excluding moves to a former club',*get(f,'R1 exclude former-club destinations','same'),src=f)
f='rev2_identification_results.csv'
add('mechanism','Own agency (twin-agency sample)',*get(f,'R2 twin sample: own + twin agencies','own agency'),src=f)
add('mechanism','Twin agencies with similar portfolios (placebo)',*get(f,'R2 twin sample: own + twin agencies','twin agencies (placebo)'),src=f)
add('mechanism','Difference: own minus twin agencies',*get(f,'R2 twin sample: own + twin agencies','difference (own - twin)'),src=f)
f='rev2c_closest_twin_results.csv'
add('mechanism','Closest single twin (placebo)',*get(f,'R2c own + closest single twin','closest twin (placebo)'),src=f)
add('mechanism','Difference: own minus closest single twin',*get(f,'R2c own + closest single twin','difference (own - closest twin)'),src=f)
f='rev2b_twin_by_contract_results.csv'
add('mechanism','Own agency, contract expiring',*get(f,'R2 twin placebo, expiring contract','own agency'),src=f)
add('mechanism','Own agency, contract runs longer',*get(f,'R2 twin placebo, contract runs longer','own agency'),src=f)
add('mechanism','Twin agencies, contract runs longer (placebo)',*get(f,'R2 twin placebo, contract runs longer','twin agencies (placebo)'),src=f)
add('mechanism','Difference, contract runs longer: own minus twin agencies',*get(f,'R2 twin placebo, contract runs longer','difference (own - twin)'),src=f)
add('mechanism','Twin agencies, contract expiring (placebo)',*get(f,'R2 twin placebo, expiring contract','twin agencies (placebo)'),src=f)
add('mechanism','Difference, contract expiring: own minus twin agencies',*get(f,'R2 twin placebo, expiring contract','difference (own - twin)'),src=f)
f='rev2_identification_results.csv'
add('mechanism','Recently placed co-client',*get(f,'R2 recent vs long-standing co-clients','recently placed co-client'),src=f)
add('mechanism','Long-standing co-client',*get(f,'R2 recent vs long-standing co-clients','long-standing co-client'),src=f)
# sensitivity
f='rev8_fill_results.csv'
add('sensitivity','First-version linkage (1,514 events)',*get(f,'V first-version linkage','same'),src=f)
add('sensitivity','First-version linkage, omit 30 June and 1 July',*get(f,'V first-version linkage | omit 30 June and 1 July','same'),src=f)
add('sensitivity','Player-level linkage, baseline labels only',*get(f,'V player-level linkage, baseline labels','same'),src=f)
add('sensitivity','Main sample, omit 30 June and 1 July',*get(f,'V MAIN: + carry-forward labels | omit 30 June and 1 July','same'),src=f)
add('sensitivity','Main sample, no club fixed effects',*get(f,'V MAIN | no club fixed effects','same'),src=f)
add('sensitivity','Main sample, harmonised representative labels',*get(f,'V MAIN | harmonised labels','same'),src=f)
add('sensitivity','Adding look-ahead labels from later releases',*get(f,'V + look-ahead labels (later releases)','same'),src=f)
f='rev4_choiceset_results.csv'
add('sensitivity','Tier and affordability controls',*get(f,'R4 + tier/affordability fit controls','same'),src=f)
add('sensitivity','Alternatives within a 4.5-fold squad-value gap',*get(f,'R4 alternatives within |value gap|<=1.5','same'),src=f)
f='rev4b_elo_results.csv'
add('sensitivity','Same 1,643 events, all clubs as alternatives',*get(f,'R4b same events, all alternatives, no fit controls','same'),src=f)
add('sensitivity','Same 1,643 events, all clubs, + tier controls',*get(f,'R4b same events, all alternatives, fit controls','same'),src=f)
add('sensitivity','Same 1,643 events, alternatives within 200 Elo',*get(f,'R4b restricted alternatives, no fit controls','same'),src=f)
add('sensitivity','Same 1,643 events, within 200 Elo, + tier controls',*get(f,'R4b restricted alternatives, fit controls','same'),src=f)
d=pd.read_csv(here/'rev5_league_results.csv')
for m,l in [('first-version sample: baseline (agency log count, local nationality)','League model, first-version sample (per doubling)'),
            ('main sample: + nationality x destination-league FE','League model, main sample, nationality-by-league effects (per doubling)')]:
    r=d[(d.model==m)&(d.term=='agency_log')].iloc[0]
    rows.append(dict(block='league',label=l,events=int(r.events),odds_ratio=r.or_per_doubling,lo=r.lo_doubling,hi=r.hi_doubling,source='rev5_league_results.csv'))
r=d[(d.model=='main sample: agency share + nationality x league FE')&(d.term=='agency_share')].iloc[0]
rows.append(dict(block='league',label='League model, main sample, agency concentration (per 10 pp of clients), nationality-by-league effects',events=int(r.events),odds_ratio=r.odds_ratio,lo=r.label_lo,hi=r.label_hi,source='rev5_league_results.csv'))
R=pd.DataFrame(rows); R.to_csv(out/'revision_results.csv',index=False); print(R.round(3).to_string())
# Figure 1: main-sample specification curve
base=pd.read_csv(out/'model_results.csv'); b=base[base.term=='same']
spec=[('Roster coverage only','Club baseline'),('+ nationality and fit','Club adjusted'),('+ club fixed effects','Club fixed effects'),('Club-by-summer FE','Club by summer fixed effects'),
      ('+ three-year flows (preferred)','Club past flows'),('Club-by-summer FE + flows','Club by summer past flows'),('Omit 30 June / 1 July + flows','Club past flows excluding turn of July')]
items=[(l,*[b[b.model==m].iloc[0][k] for k in ('odds_ratio','label_lo','label_hi')],'main') for l,m in spec]
for r in R[R.block=='sensitivity'].itertuples():
    if r.label.startswith('Main sample, omit') : continue
    items.append((r.label,r.odds_ratio,r.lo,r.hi,'sens'))
fig,ax=plt.subplots(figsize=(7.2,6.2)); y=np.arange(len(items))[::-1]
for yi,(l,o,lo,hi,k) in zip(y,items):
    col='#1f4e79' if k=='main' else '#b5651d'
    ax.plot([lo,hi],[yi,yi],color=col,lw=1.6); ax.plot(o,yi,'o',color=col,ms=5)
ax.axvline(1,color='#666',lw=.8,ls='--'); ax.set_yticks(y); ax.set_yticklabels([i[0] for i in items],fontsize=8)
ax.set_xlabel('Odds ratio, co-client at destination club (95% CI)',fontsize=8); ax.tick_params(axis='x',labelsize=8); ax.set_xlim(0.85,2.3)
for s in ('top','right'): ax.spines[s].set_visible(False)
plt.tight_layout(); plt.savefig(out/'figureA1_specifications.png',dpi=200); print('figure A1 saved')

# ---- compact main figure: preferred estimate, own vs twin estimates and direct contrasts
def g(f,m,t): return get(f,m,t)[1:]
pref=[b[b.model=='Club past flows'].iloc[0][k] for k in ('odds_ratio','label_lo','label_hi')]
f2='rev2_identification_results.csv'; f3='rev2c_closest_twin_results.csv'
its=[('Preferred club model, 2,000 moves',*pref,'a'),
     ('Own agency, joint model (1,674 moves)',*g(f2,'R2 twin sample: own + twin agencies','own agency'),'a'),
     ('Three matched agencies, same model',*g(f2,'R2 twin sample: own + twin agencies','twin agencies (placebo)'),'b'),
     ('Ratio: own to three matched agencies',*g(f2,'R2 twin sample: own + twin agencies','difference (own - twin)'),'c'),
     ('Closest matched agency, separate model',*g(f3,'R2c own + closest single twin','closest twin (placebo)'),'b'),
     ('Ratio: own to closest matched agency',*g(f3,'R2c own + closest single twin','difference (own - closest twin)'),'c')]
col={'a':'#1f4e79','b':'#b5651d','c':'#444444'}
fig,ax=plt.subplots(figsize=(7.2,3.4)); y=np.arange(len(its))[::-1]
for yi,(l,o,lo,hi,k) in zip(y,its):
    ax.plot([lo,hi],[yi,yi],color=col[k],lw=1.8); ax.plot(o,yi,'D' if k=='c' else 'o',color=col[k],ms=5.5)
ax.axvline(1,color='#666',lw=.8,ls='--'); ax.set_yticks(y); ax.set_yticklabels([i[0] for i in its],fontsize=8.5)
ax.set_xlabel('Odds ratio or ratio of odds ratios (95% CI); null value = 1',fontsize=8.5); ax.tick_params(axis='x',labelsize=8.5); ax.set_xlim(0.75,2.0)
for sp in ('top','right'): ax.spines[sp].set_visible(False)
plt.tight_layout(); plt.savefig(out/'figure1_main.png',dpi=200); print('figure 1 saved')
