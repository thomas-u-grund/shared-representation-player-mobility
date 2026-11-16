"""Elo restriction decomposition: same events, original alternatives vs restricted alternatives (with and without fit controls)."""
import warnings; warnings.filterwarnings('ignore')
import re
src=open('rev4_choiceset.py').read()
head=src[:src.index("# restrict to clubs whose Elo")]
head=head.replace("ns['records'].clear()\nfit('R4 main sample'","ns['records'].clear()\n#fit('R4 main sample'").replace("fit('R4 + tier","#fit('R4 + tier")
head=re.sub(r"for w in \(1.5,1.0\):.*?fit\(f'R4 alternatives within \|value gap\|<=\{w\}'.*?\*D\)\n","",head,flags=re.S)
exec(head)
ek = m[(m.abs_elo_gap<=2)|m.elo_gap_unknown.eq(1)]
ns['records'].clear()
fit('R4b restricted alternatives, fit controls',ek,['same']+controls+flows+fitc,*D)
fit('R4b restricted alternatives, no fit controls',ek,['same']+controls+flows,*D)
ids=set(ek[ek.chosen==1].event_id)
same_events=m[m.event_id.isin(ids)]
fit('R4b same events, all alternatives, no fit controls',same_events,['same']+controls+flows,*D)
fit('R4b same events, all alternatives, fit controls',same_events,['same']+controls+flows+fitc,*D)
r=pd.DataFrame(ns['records']); r.to_csv('rev4b_elo_results.csv',index=False)
pd.set_option('display.width',200)
print(r[r.term=='same'][['model','events','alternatives','odds_ratio','label_lo','label_hi']].round(3).to_string())
