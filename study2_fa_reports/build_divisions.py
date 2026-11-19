# Assigns each registering club in each FA reporting period a division (PL / Championship / League One / League Two / National League System).
# Sources, in order: (1) the report's own section label where it has one (2020-21, 2023-24 onward); (2) Transfermarkt-derived season membership of the EFL and the
# National League (sptd_lower_club_league_history.csv, SPTD version 2, season s = start year of season s-(s+1)); (3) Premier League membership for each season entered by hand from the
# public league tables (the SPTD history does not cover the Premier League); (4) clubs labelled "National League System" in any report are NLS in seasons where they are not in the EFL or the National League.
# Everything else is left unresolved. Validation: sources (2) and (3) are compared with the report's own labels in 2020-21 and 2023-24.
import csv, collections, re, unicodedata, json, sys
# Usage: python3 -I build_divisions.py <folder with sptd_clubs.csv and sptd_lower_club_league_history.csv>   (Transfermarkt-derived season membership of the EFL and the National League; not distributed,
# see README.md). The result, club_division.csv, is shipped, so this step is only needed to rebuild it.
def norm(s):
    s = unicodedata.normalize('NFKD', s).encode('ascii', 'ignore').decode().lower()
    s = re.sub(r'\(.*?\)', ' ', s); s = re.sub(r'[^a-z0-9& ]+', ' ', s)
    return ' '.join(x for x in s.split() if x not in ('fc', 'afc', 'cf', 'a', 'f', 'c', 'the', 'ltd', 'limited'))
L = sys.argv[1].rstrip('/') + '/'
names = {r['id']: r['club_name'] for r in csv.DictReader(open(L + 'sptd_clubs.csv', encoding='utf8'))}
H = list(csv.DictReader(open(L + 'sptd_lower_club_league_history.csv')))
LG = {'GB2': 'EFL Championship', 'GB3': 'EFL League One', 'GB4': 'EFL League Two', 'CNAT': 'National League System'}
ALIAS = json.load(open('club_aliases.json')) if __import__('os').path.exists('club_aliases.json') else {}
def key(s): n = norm(s); return ALIAS.get(n, n)
tm = collections.defaultdict(dict)
for r in H:
    if r['league_id'] in LG: tm[int(r['season'])][key(names.get(r['club_id'], r['club_id']))] = LG[r['league_id']]
PL = {  # Premier League members by season start year, from the public league tables (premierleague.com / Wikipedia season pages)
 2019: "arsenal|aston villa|bournemouth|brighton & hove albion|burnley|chelsea|crystal palace|everton|leicester city|liverpool|manchester city|manchester united|newcastle united|norwich city|sheffield united|southampton|tottenham hotspur|watford|west ham united|wolverhampton wanderers",
 2020: "arsenal|aston villa|brighton & hove albion|burnley|chelsea|crystal palace|everton|fulham|leeds united|leicester city|liverpool|manchester city|manchester united|newcastle united|sheffield united|southampton|tottenham hotspur|west bromwich albion|west ham united|wolverhampton wanderers",
 2021: "arsenal|aston villa|brentford|brighton & hove albion|burnley|chelsea|crystal palace|everton|leeds united|leicester city|liverpool|manchester city|manchester united|newcastle united|norwich city|southampton|tottenham hotspur|watford|west ham united|wolverhampton wanderers",
 2022: "afc bournemouth|arsenal|aston villa|brentford|brighton & hove albion|chelsea|crystal palace|everton|fulham|leeds united|leicester city|liverpool|manchester city|manchester united|newcastle united|nottingham forest|southampton|tottenham hotspur|west ham united|wolverhampton wanderers",
 2023: "afc bournemouth|arsenal|aston villa|brentford|brighton & hove albion|burnley|chelsea|crystal palace|everton|fulham|liverpool|luton town|manchester city|manchester united|newcastle united|nottingham forest|sheffield united|tottenham hotspur|west ham united|wolverhampton wanderers"}
MANUAL = {  # clubs now in the Premier League or otherwise missing from the SPTD history; divisions from the public league tables of those seasons
 (2019, 'brentford'): 'EFL Championship', (2019, 'fulham'): 'EFL Championship', (2019, 'nottingham forest'): 'EFL Championship', (2019, 'ipswich town'): 'EFL League One',
 (2019, 'macclesfield town'): 'EFL League Two', (2021, 'bournemouth'): 'EFL Championship', (2021, 'fulham'): 'EFL Championship', (2021, 'nottingham forest'): 'EFL Championship',
 (2021, 'ipswich town'): 'EFL League One', (2022, 'ipswich town'): 'EFL League One'}
ENGLISH_NONLEAGUE_BY_ELIMINATION = {  # English clubs named in the reports that are in neither the Premier League nor the EFL in that season (the reports cover only the PL, EFL and National League System)
 '2019-20': {'bradford park avenue', 'carshalton athletic', 'grays athletic', 'maldon and tipree', 'ramsgate', 'tooting & mitcham united', 'trafford'}}
PLK = {s: {key(c) for c in v.split('|')} for s, v in PL.items()}
fa = list(csv.DictReader(open('fa_transactions_long.csv', encoding='utf8')))
PER = sorted(set(x['period'] for x in fa))
lab = {}  # (period, club) -> section
for x in fa:
    if x['section'] and 'Women' not in x['section']: lab.setdefault((x['period'], key(x['registering_club'])), collections.Counter())[x['section']] += 1
lab = {k: v.most_common(1)[0][0] for k, v in lab.items()}
nls_ever = {c for (p, c), s in lab.items() if s == 'National League System'}
rows = []; val = collections.Counter(); mism = []
for p in PER:
    s = int(p[:4]); clubs = sorted(set(key(x['registering_club']) for x in fa if x['period'] == p))
    for c in clubs:
        own = lab.get((p, c)); src = div = None
        if s in PLK and c in PLK[s]: div, src = 'Premier League', 'public table'
        elif c in tm.get(s, {}): div, src = tm[s][c], 'SPTD season membership'
        elif (s, c) in MANUAL: div, src = MANUAL[(s, c)], 'public league table (hand-entered)'
        if own:
            if div and p in ('2020-21', '2023-24', '2024-25', '2025-26'):
                val[(p, div == own)] += 1
                if div != own: mism.append((p, c, own, div))
            div, src = own, 'FA report section'
        elif div is None and c in nls_ever and s <= 2025: div, src = 'National League System', 'labelled NLS in another report'
        elif div is None and (p in ('2021-22', '2022-23') or c in ENGLISH_NONLEAGUE_BY_ELIMINATION.get(p, ())): div, src = 'National League System', 'English club outside PL/EFL (by elimination)'
        rows.append({'period': p, 'club_k': c, 'division': div or '', 'source': src or 'unresolved'})
with open('club_division.csv', 'w', newline='', encoding='utf8') as f:
    w = csv.DictWriter(f, fieldnames=['period', 'club_k', 'division', 'source']); w.writeheader(); w.writerows(rows)
print('validation of derived divisions against the reports own labels (period, agrees):', dict(val))
print('disagreements:', mism[:40])
c = collections.Counter((r['period'], r['source']) for r in rows)
for p in PER: print(p, {k[1]: v for k, v in c.items() if k[0] == p})
