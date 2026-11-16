"""Descriptive conditional logits for league and club destination."""
import csv,re,unicodedata
from collections import defaultdict,Counter
import numpy as np
from scipy.optimize import minimize
from scipy.special import softmax,logsumexp
def read(p):
    with open(p) as f:return list(csv.DictReader(f))
def key(s):
    return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKD',s.casefold()).encode('ascii','ignore').decode())
roster=read('player_snapshots.csv')
events=[e for e in read('summer_transfers.csv') if e['known_agent']=='True']
labels={(r['summer'],r['player_id'],r['club_id']):key(r['agent_label']) for r in roster}
market=Counter((r['summer'],r['league'],key(r['agent_label'])) for r in roster if r['agent_label'])
club=Counter((r['summer'],r['club_id'],key(r['agent_label'])) for r in roster if r['agent_label'])
clubs=defaultdict(set)
# Prior-season clubs with an observed agency roster, classified by next-season league.
for e in read('destination_clubs.csv'):
    clubs[e['summer'],e['league']].add(e['club_id'])
known=Counter((r['summer'],r['club_id']) for r in roster if r['agent_label'])
leagues=sorted({r['league'] for r in roster if r['summer']=='2023'})
results=[]
for sample in ('broad','return_screened'):
    for model in ('league','cross_league','club_within_destination_league'):
        risk=[]
        for e in events:
            if sample=='return_screened' and e['likely_return']=='True':continue
            y,p,c,d,h,l=(e[k] for k in ('summer','player_id','origin_club','destination_club','origin_league','destination_league'))
            a=labels[y,p,c]
            if model=='cross_league' and h==l:continue
            if model=='club_within_destination_league':
                candidates=sorted(b for b in clubs[y,l] if b!=c and known[y,b])
                if d not in candidates or len(candidates)<2:continue
                x=[[float(club[y,b,a]>0),np.log1p(known[y,b])] for b in candidates]
                chosen=d
            else:
                candidates=[g for g in leagues if model=='league' or g!=h]
                x=[[np.log1p(max(0,market[y,g,a]-(g==h))),*([int(g==h)] if model=='league' else []),*[int(g==s) for s in leagues[1:]]] for g in candidates]
                chosen=l
            risk.append((p,np.array(x),candidates.index(chosen)))
        k=risk[0][1].shape[1]
        X=np.zeros((len(risk),max(len(x) for _,x,_ in risk),k))
        mask=np.full(X.shape[:2],-np.inf)
        Y=np.array([x[i] for _,x,i in risk])
        for j,(_,x,_) in enumerate(risk):
            X[j,:len(x)]=x
            mask[j,:len(x)]=0
        def calc(b):
            v=X@b+mask
            prob=softmax(v,axis=1)
            return v,prob,np.einsum('ij,ijk->ik',prob,X)
        def obj(b):
            v,_,m=calc(b)
            return np.sum(logsumexp(v,axis=1)-Y@b),(m-Y).sum(axis=0)
        opt=minimize(obj,np.zeros(k),jac=True,method='BFGS')
        assert np.max(np.abs(obj(opt.x)[1]))<1e-3
        _,pr,m=calc(opt.x)
        bread=np.linalg.inv(np.einsum('ij,ijk,ijl->kl',pr,X,X)-m.T@m)
        scores=defaultdict(lambda:np.zeros(k))
        for (pid,_,_),s in zip(risk,Y-m):scores[pid]+=s
        S=np.array(list(scores.values()))
        se=np.sqrt(np.diag(bread@(S.T@S)@bread)*len(S)/(len(S)-1))[0]
        b=opt.x[0]
        results.append(dict(sample=sample,model=model,events=len(risk),odds_ratio=np.exp(b),ci_low=np.exp(b-1.96*se),ci_high=np.exp(b+1.96*se)))
with open('model_results.csv','w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=list(results[0]));w.writeheader();w.writerows(results)
for row in results:print(row)
