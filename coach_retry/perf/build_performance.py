"""Enrich the 14-league transfer panel with pre-window performance."""
import csv
import gzip
import io
import re
import sys
import unicodedata
import zipfile
from collections import Counter, defaultdict
from datetime import date, datetime
from pathlib import Path
from statistics import median

base, raw, out = map(Path,sys.argv[1:])
out.mkdir(exist_ok=True)
country=dict(zip('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1'.split(),
                 'BEL DEN ESP FRA ENG GRE ITA GER NED POR RUS SCO TUR UKR'.split()))
versions={'2023':'240','2024':'435','2025':'600'}

def read(path, encoding='utf-8-sig'):
    opener=gzip.open if path.suffix=='.gz' else open
    with opener(path,'rt',encoding=encoding) as f:
        yield from csv.DictReader(f)

def archive(path, member):
    if zipfile.is_zipfile(path):
        with zipfile.ZipFile(path) as z:
            yield from csv.DictReader(io.TextIOWrapper(z.open(member),encoding='utf-8-sig'))
    else:
        yield from read(path)

def write(name, rows):
    with (out/name).open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)

def folded(s):
    return unicodedata.normalize('NFKD', s.casefold()).encode('ascii','ignore').decode()

def key(s):
    return re.sub('[^a-z0-9]','',folded(s))

def club_key(s):
    return key(re.sub(r'\b(?:football club|fussball club|gesellschaft mit beschrankter haftung|fc|cf|afc|ac|ssc|sc|as|sv|vfb|vfl|tsg|fsv|ogc|rc|losc|calcio|spa|sad|gmbh)\b',' ',folded(s)))

def int0(s):
    return int(float(s or 0))

snapshots=[r for r in read(base/'player_snapshots.csv') if r['summer'] in versions]
attrs={}
for year,version in versions.items():
    attrs.update({(year,r['player_id']):r for r in archive(base/(version+'.data'),'players.csv')})
games={}
teams=defaultdict(lambda:dict(games=0,points=0,goals_for=0,goals_against=0))
team_names=defaultdict(Counter)
next_names=defaultdict(Counter)
team_league={}
for g in read(base/'games.gz'):
    league=g['competition_id']
    if league not in country or not g['season'].isdigit():continue
    year=str(int(g['season'])+1)
    if year not in versions:
        entering=str(int(g['season']))
        if entering in versions:
            for side in ('home','away'):
                next_names[entering,g[side+'_club_id']].update([g[side+'_club_name']])
        continue
    entering=str(int(g['season']))
    if entering in versions:
        for side in ('home','away'):
            next_names[entering,g[side+'_club_id']].update([g[side+'_club_name']])
    if not g['date'] or g['date']>=year+'-06-01' or not all(g[s+'_club_goals'].isdigit() for s in ('home','away')):continue
    games[g['game_id']]=year,g['home_club_id'],g['away_club_id']
    for side,other in [('home','away'),('away','home')]:
        c=g[side+'_club_id']
        gf,ga=int(g[side+'_club_goals']),int(g[other+'_club_goals'])
        t=teams[year,c]
        t['games']+=1
        t['points']+=3 if gf>ga else int(gf==ga)
        t['goals_for']+=gf
        t['goals_against']+=ga
        team_names[year,c].update([g[side+'_club_name']])
        team_league[year,c]=league
for r in snapshots:
    a=attrs[r['summer'],r['player_id']]
    if a.get('current_club_name'):team_names[r['summer'],r['club_id']].update([a['current_club_name']])

stats=defaultdict(Counter)
appearance_clubs=Counter()
seen=set()
for r in read(raw/'appearances.gz'):
    g=games.get(r['game_id'])
    if not g or r['player_club_id'] not in g[1:] or r['appearance_id'] in seen:continue
    seen.add(r['appearance_id'])
    year=g[0]; c=r['player_club_id']; p=r['player_id']
    m=int0(r['minutes_played'])
    stats[year,c,p].update(matches=int(m>0),minutes=m,goals=int0(r['goals']),assists=int0(r['assists']))
    appearance_clubs[year,c]+=1

game23=defaultdict(set)
for r in read(raw/'2023.ratings.csv'):
    for n in (r['Name'],r['FullName']):
        if n:game23[key(n)].add((r['ID'],r['Overall'],r['Age']))
game24=defaultdict(set)
for r in read(raw/'2024.ratings.csv','latin-1'):
    dob=r['dob']
    if '/' in dob:
        try:dob=datetime.strptime(dob,'%m/%d/%Y').date().isoformat()
        except ValueError:continue
    game24[key(r['name']),dob[:10]].add((r['player_id'],r['overall_rating']))
game25=defaultdict(set)
for r in archive(raw/'sofifa.zip','all_fifa_players.csv'):
    m=re.match(r'^\d+',r['Overall Score'])
    if m:game25[r['Player ID']].add(m[0])

players=[]
squad=defaultdict(list)
for r in snapshots:
    year,c,p=r['summer'],r['club_id'],r['player_id']
    a=attrs[year,p]; dob=a.get('date_of_birth','')[:10]
    n={key(a['name']),key(a.get('first_name','')+' '+a.get('last_name',''))}-{''}
    age=round((date.fromisoformat(year+'-06-01')-date.fromisoformat(dob)).days/365.25,2) if re.fullmatch(r'\d{4}-\d\d-\d\d',dob) else ''
    if year=='2023':
        hits={(i,o) for k in n for i,o,g_age in game23[k] if dob and g_age.isdigit() and 2022-int(dob[:4])-int(g_age) in (0,1)}
        method='fifa23_name_age'
    else:
        hits={pair for k in n for pair in game24[k,dob] if dob}
        method='fc24_name_dob'
    gid,rating=next(iter(hits)) if len(hits)==1 else ('','')
    if year=='2025':
        values=game25[gid]
        rating=next(iter(values)) if len(values)==1 else ''
        method='fc25_archive_via_fc24_id'
    pos=a.get('position','')
    s=stats[year,c,p]
    m=s['minutes'] if appearance_clubs[year,c] else ''
    row=dict(**r,position=pos,sub_position=a.get('sub_position',''),nationality=a.get('country_of_citizenship',''),
             age_at_cutoff=age,market_value_eur=a.get('market_value_in_eur',''),prior_league_minutes=m,
             prior_league_appearances=s['matches'] if m!='' else '',prior_league_goals=s['goals'] if m!='' else '',
             prior_league_assists=s['assists'] if m!='' else '',ea_id=gid,ea_overall=rating,
             rating_match=method if rating else '')
    players.append(row)
    if pos and pos not in {'Missing','-'}:squad[year,c,pos].append(row)

elo=defaultdict(list)
for r in read(raw/'elo.csv'):
    y=r['date'][:4]
    if y in versions and y+'-05-01'<=r['date']<y+'-06-01':
        elo[y,r['country'],club_key(r['club'])].append((r['date'],r['club'],float(r['elo'])))
aliases={}
alias_path=raw/'elo_aliases.csv'
if alias_path.exists():
    aliases={(r['country'],club_key(r['transfermarkt'])):club_key(r['elo']) for r in read(alias_path)}
destinations={(r['summer'],r['club_id']):r['league'] for r in read(base/'destination_clubs.csv') if r['summer'] in versions}
ppg_groups=defaultdict(list)
for k,t in teams.items():
    ppg_groups[k[0],team_league[k]].append(t['points']/t['games'])
club_rows={}
for year,c in sorted(destinations.keys()|teams.keys()):
    prior=team_league.get((year,c),'')
    nxt=destinations.get((year,c),'')
    t=teams.get((year,c))
    ppg=t['points']/t['games'] if t else ''
    group=ppg_groups[year,prior]
    pct=(sum(v<ppg for v in group)+0.5*(sum(v==ppg for v in group)-1))/(len(group)-1) if len(group)>1 else ''
    ct=country.get(prior or nxt,'')
    if not team_names[year,c]:team_names[year,c].update(next_names[year,c])
    found=defaultdict(list)
    for name in team_names[year,c]:
        ck=club_key(name)
        for day,team,value in elo[year,ct,aliases.get((ct,ck),ck)]:
            found[team].append((day,value))
    label=next(iter(found)) if len(found)==1 else ''
    ed,ev=max(found[label]) if label else ('','')
    club_rows[year,c]=dict(summer=year,club_id=c,club=team_names[year,c].most_common(1)[0][0] if team_names[year,c] else '',
            prior_league=prior,next_league=nxt,entered_tracked_top_flight=bool(nxt and not prior),
            matches=t['games'] if t else '',points=t['points'] if t else '',points_per_match=ppg,
            ppg_percentile=pct,goal_difference=t['goals_for']-t['goals_against'] if t else '',
            club_elo=ev,elo_date=ed,elo_label=label)

player_by_id={(r['summer'],r['player_id'],r['club_id']):r for r in players}
club_rosters=defaultdict(list)
for row in players:
    club_rosters[row["summer"],row["club_id"]].append(row)
moves=[]; choices=[]
for i,e in enumerate(read(base/'summer_transfers.csv'),1):
    y,p,o,d=(e[x] for x in ('summer','player_id','origin_club','destination_club'))
    pl=player_by_id.get((y,p,o),{})
    c0=club_rows[y,o]; c1=club_rows[y,d]
    delta=c1['club_elo']-c0['club_elo'] if c1['club_elo']!='' and c0['club_elo']!='' else ''
    moves.append(dict(event_id=i,**e,origin_ppg=c0['points_per_match'],destination_ppg=c1['points_per_match'],
        origin_elo=c0['club_elo'],destination_elo=c1['club_elo'],elo_change=delta,
        player_position=pl.get('position',''),player_age=pl.get('age_at_cutoff',''),
        player_minutes=pl.get('prior_league_minutes',''),player_ea_overall=pl.get('ea_overall','')))
    if e['known_agent']!='True':continue
    assert pl
    label=key(pl['agent_label'])
    for (sy,c),league in sorted(destinations.items()):
        if sy!=y or league!=e['destination_league'] or c==o:continue
        team=club_rows[y,c]
        peers=[r for r in squad[y,c,pl['position']] if r['player_id']!=p]
        rated=[float(r['ea_overall']) for r in peers if r['ea_overall']!='']
        observed=[r['prior_league_minutes'] for r in peers if r['prior_league_minutes']!='']
        choices.append(dict(event_id=i,summer=y,candidate_club=c,candidate_league=league,chosen=int(c==d),
            candidate_club_elo=team['club_elo'],candidate_points_per_match=team['points_per_match'],
            candidate_ppg_percentile=team['ppg_percentile'],candidate_entered_top_flight=team['entered_tracked_top_flight'],
            co_clients=sum(r['player_id']!=p and key(r['agent_label'])==label for r in club_rosters[y,c]),
            candidate_roster_observed=bool(club_rosters[y,c]),
            same_position_players=len(peers) if club_rosters[y,c] and pl['position'] else '',
            same_position_minutes=sum(observed) if observed else '',
            same_position_rated_players=len(rated),
            same_position_median_rating=median(rated) if rated else '',
            player_minus_median_rating=float(pl['ea_overall'])-median(rated) if rated and pl['ea_overall'] else ''))
write('player_performance.csv',players)
write('club_performance.csv',list(club_rows.values()))
write('transfer_performance.csv',moves)
write('within_league_candidates.csv',choices)
audit=[]
for y in versions:
    ps=[r for r in players if r['summer']==y]
    cs=[club_rows[k] for k in destinations if k[0]==y]
    es=[r for r in moves if r['summer']==y and r['known_agent']=='True' and r['likely_return']=='False']
    chosen=[r for r in choices if r['chosen'] and r['summer']==y]
    row=dict(summer=y,players=len(ps),player_minutes=sum(r['prior_league_minutes']!='' for r in ps),
             rated_players=sum(bool(r['ea_overall']) for r in ps),destination_clubs=len(cs),
             clubs_with_ppg=sum(r['points_per_match']!='' for r in cs),
             clubs_with_elo=sum(r['club_elo']!='' for r in cs),screened_transfers=len(es),
             screened_elo_change=sum(r['elo_change']!='' for r in es),
             screened_player_rating=sum(bool(r['player_ea_overall']) for r in es),
             chosen_with_rating_fit=sum(r['player_minus_median_rating']!='' for r in chosen))
    audit.append(row)
    print(row)
write('coverage.csv',audit)
assert len(moves)==4706 and len(players)==19549
assert sum(r['chosen'] for r in choices)==2721
