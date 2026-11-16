import csv,gzip,zipfile,io
from collections import Counter,defaultdict
L=set('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1'.split())
V={2023:240,2024:435,2025:600,2026:660}
def zipped(n):
 with gzip.open(n+'.gz','rt') as f: yield from csv.DictReader(f)
clubs=defaultdict(dict)
for g in zipped('games'):
 if g['competition_id'] in L and g['season'].isdigit():
  for side in ('home','away'):
   clubs[int(g['season'])][g[side+'_club_id']]=g['competition_id']
print('league coverage',[(s, len(set(c.values())),len(c)) for s,c in sorted(clubs.items()) if 2022<=s<=2026])
observed={}
profiles=[]
league_counts=Counter()
for y,v in V.items():
 p=f'{v}.data'
 if zipfile.is_zipfile(p):
  with zipfile.ZipFile(p) as z: data=z.read('players.csv').decode('utf-8-sig')
 else:data=open(p,encoding='utf-8-sig').read()
 rr=[r for r in csv.DictReader(io.StringIO(data)) if r['last_season']==str(y-1) and (r.get('current_club_domestic_competition_id') or r.get('domestic_competition_id')) in L or y==2026 and r['last_season']==str(y-1) and r.get('current_club_domestic_competition_id')]
 observed[y]={(r['player_id'],r['current_club_id']):(r.get('agent_name') or '').strip().lower() not in {'','null','none','-','nan','n/a','unknown','without agent'} for r in rr}
 for r in rr:
  league=r.get('current_club_domestic_competition_id') or r.get('domestic_competition_id')
  known=observed[y][r['player_id'],r['current_club_id']]
  profiles.append(dict(summer=y,version=v,league=league,player_id=r['player_id'],club_id=r['current_club_id'],player=r['name'],agent_label=r['agent_name'] if known else ''))
  league_counts[y,league,'roster']+=1
  league_counts[y,league,'named']+=known
 print(y,'roster',len(rr),sum(observed[y].values()))
raw=set()
hist=defaultdict(list)
for r in zipped('transfers'):
 d,p,a,b=r['transfer_date'],r['player_id'],r['from_club_id'],r['to_club_id']
 if not a or not b or a==b: continue
 row=d,p,a,b
 hist[p,b,a].append(d)
 if (y:=int(d[:4])) in V and f'{y}-06-01'<=d<=f'{y}-09-01':
  raw.add((*row,r['transfer_fee']))
summary=Counter()
events=[]
dedup=defaultdict(set)
for d,p,a,b,fee in raw:dedup[d,p,a,b].add(fee)
for (d,p,a,b),fees in dedup.items():
 y=int(d[:4])
 if y==2026:continue
 A=clubs[y-1].get(a); B=clubs[y].get(b)
 if not A or not B:continue
 t='within' if A==B else 'between'
 obs=observed[y].get((p,a))
 return_proxy=d[5:] in {'06-30','07-01'} and all(not x or float(x)==0 for x in fees) and any(f'{y-1}-06-01'<=x<d for x in hist[p,a,b])
 events.append(dict(summer=y,date=d,player_id=p,origin_club=a,destination_club=b,origin_league=A,destination_league=B,route=t,baseline_match=obs is not None,known_agent=obs is True,likely_return=return_proxy))
 for g,on in [('all',True),('roster',obs is not None),('agent',obs is True),('agent_screened',obs is True and not return_proxy)]:
  if on:summary[y,g,t]+=1
for y in (2023,2024,2025):
 print(y,[(g,summary[y,g,'within'],summary[y,g,'between']) for g in ('all','roster','agent','agent_screened')])

def save(name, rows):
 with open(name,'w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
save('player_snapshots.csv',profiles)
save('summer_transfers.csv',events)
save('counts.csv',[dict(summer=y,screen=g,within=summary[y,g,'within'],between=summary[y,g,'between'])
 for y in (2023,2024,2025) for g in ('all','roster','agent','agent_screened')])
save('league_coverage.csv',[dict(summer=y,league=l,players=league_counts[y,l,'roster'],named_agent=league_counts[y,l,'named']) for y in V for l in sorted({x['league'] for x in profiles if x['summer']==y})])
save('destination_clubs.csv',[dict(summer=y,club_id=c,league=l) for y in (2023,2024,2025) for c,l in clubs[y].items()])
