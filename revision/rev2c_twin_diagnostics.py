"""Twin-model diagnostics: sample flow 2,000 -> 1,674, own/twin cross-tabulation, closest-single-twin model."""
import warnings; warnings.filterwarnings('ignore')
src=open('rev2_identification.py').read()
exec(src[:src.index("ns['records'].clear()\nfit('R2 twin sample: own agency only'")])
valid=ns['valid']
def retained(x,fixed='candidate_club'):
    x=valid(x)
    while True:
        n=len(x); x=valid(x[x[fixed].isin(x.loc[x.chosen.eq(1),fixed])])
        if n==len(x): break
    return x
base=retained(main)
nA=main.event_id.nunique(); nB=base.event_id.nunique()
print('events in club candidate file (non-return, non-conversion):',nA,' retained by club model:',nB)
# reason an event has no twin
nlab=pl.groupby(['summer','label']).size().to_dict()
ev1=base.drop_duplicates('event_id')[['event_id','summer','label']].copy()
ev1['has_twin']=[(s,l) in twin_of for s,l in zip(ev1.summer,ev1.label)]
ev1['label_clients']=[nlab.get((s,l),0) for s,l in zip(ev1.summer,ev1.label)]
no_t=ev1[~ev1.has_twin]
r_small=int((no_t.label_clients<3).sum()); r_elig=int((no_t.label_clients>=3).sum())
withtw=base[base.event_id.isin(ev1.event_id[ev1.has_twin])]
final=retained(withtw)
nC=ev1.has_twin.sum(); nF=final.event_id.nunique()
flow=[('Events in the club model',nB),('Representative has fewer than three clients on pre-window rosters, so no twin is defined',-r_small),
      ('Fewer than three agencies meet the size rule',-r_elig),('Dropped after twin restriction (single-alternative or never-chosen clubs)',-(nC-nF)),('Events in the twin model',nF)]
assert nB+(-r_small)+(-r_elig)+(-(nC-nF))==nF
pd.DataFrame(flow,columns=['step','events']).to_csv('rev2c_sample_flow.csv',index=False); print(flow)
# cross-tab of alternatives in the twin-model sample
fx=final.copy(); fx['cat']=np.select([(fx.same==1)&(fx.twin_any==0),(fx.same==0)&(fx.twin_any==1),(fx.same==1)&(fx.twin_any==1)],['Own agency only','Twin only','Both'],'Neither')
ct=fx.groupby('cat').agg(alternatives=('chosen','size'),chosen=('chosen','sum')); ct['share_of_alternatives']=ct.alternatives/ct.alternatives.sum(); ct['chosen_share']=ct.chosen/ct.alternatives
ct=ct.loc[['Own agency only','Twin only','Both','Neither']]
hasown=fx.groupby('event_id').apply(lambda g:((g.cat=='Own agency only').any(),(g.cat=='Twin only').any()))
both_ev=int(sum(a and b for a,b in hasown)); own_ev=int(sum(a for a,b in hasown)); tw_ev=int(sum(b for a,b in hasown))
ct.to_csv('rev2c_crosstab.csv'); pd.DataFrame([dict(events=nF,with_own_only=own_ev,with_twin_only=tw_ev,with_both_types=both_ev)]).to_csv('rev2c_choice_sets.csv',index=False)
print(ct.round(3)); print(own_ev,tw_ev,both_ev,nF)
# closest-single-twin model
close={k:v[0] for k,v in twin_of.items()}
fx2=final.copy(); fx2['twin1']=[float(cnt.get((s,cc,close[(s,l)]),0)>0) for s,cc,l in zip(fx2.summer,fx2.candidate_club,fx2.label)]
ns['records'].clear()
fit('R2c own + closest single twin',fx2,['same','twin1']+controls+flows,*D,{'own agency':{'same':1},'closest twin (placebo)':{'twin1':1},'difference (own - closest twin)':{'same':1,'twin1':-1}})
r=pd.DataFrame(ns['records']); r.to_csv('rev2c_closest_twin_results.csv',index=False)
print(r[r.term.isin(['own agency','closest twin (placebo)','difference (own - closest twin)'])][['events','term','odds_ratio','label_lo','label_hi']].round(3).to_string()); print('closest twin prevalence among alternatives',fx2.twin1.mean().round(3))
