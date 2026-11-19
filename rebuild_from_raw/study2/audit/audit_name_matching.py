# Audit of the name-based intermediary matching. Part A (ID years 2019-20 to 2022-23, where the registration number is the truth): false merges (same normalised name, different ID)
# and misses (same ID, different normalised name) in 100 random cross-period pairs of each kind. Part B (periods without IDs): 100 random cross-period name matches, checked for
# firm conflicts, multi-firm names and single-word names; the sample is printed so that it can be read by hand. Seed 20260419.
import csv, random, re, unicodedata, collections
def norm(s):
    s = unicodedata.normalize('NFKD', s or '').encode('ascii', 'ignore').decode().lower(); s = re.sub(r'\(.*?\)', ' ', s); s = re.sub(r'[^a-z0-9& ]+', ' ', s)
    return ' '.join(w for w in s.split() if w not in ('ltd', 'limited', 'llp', 'plc', 'the', 'uk', 'sl', 'gmbh'))
R = list(csv.DictReader(open(__import__('os').path.join(__import__('os').path.dirname(__import__('os').path.abspath(__file__)),'..','..','..','data','study2','fa_transactions_long.csv'), encoding='utf8')))
PER = sorted(set(x['period'] for x in R)); idy = ['2019-20', '2020-21', '2021-22', '2022-23']
for x in R: x['nm'] = norm(x['agent_name']); x['fm'] = norm(x['agent_firm'])
random.seed(20260419)
# ---- Part A
A = [x for x in R if x['period'] in idy and x['agent_id'] and x['nm'] not in ('', '-')]
byname = collections.defaultdict(list); byid = collections.defaultdict(list)
for x in A: byname[x['nm']].append(x); byid[x['agent_id']].append(x)
def cross(lst):
    pairs = []
    for i in range(len(lst)):
        for j in range(i + 1, len(lst)):
            if lst[i]['period'] != lst[j]['period']: pairs.append((lst[i], lst[j]))
    return pairs
pn = [p for n, l in byname.items() for p in cross(l[:60])]; pi = [p for n, l in byid.items() for p in cross(l[:60])]
sn = random.sample(pn, 100); si = random.sample(pi, 100)
fm = sum(a['agent_id'] != b['agent_id'] for a, b in sn); ms = sum(a['nm'] != b['nm'] for a, b in si)
print(f'A1: of 100 random cross-period name matches, {fm} link different registration numbers (false merges)')
print(f'A2: of 100 random cross-period same-ID pairs, {ms} have different normalised names (the name rule would miss them)')
names_multi = sum(len({y["agent_id"] for y in l}) > 1 for l in byname.values()); ids_multi = sum(len({y["nm"] for y in l}) > 1 for l in byid.values())
print(f'A3: all normalised names in ID years: {len(byname)}; with >1 ID: {names_multi}; all IDs: {len(byid)}; with >1 name: {ids_multi}')
ex = [(a['agent_name'], a['agent_id'], b['agent_name'], b['agent_id']) for a, b in sn if a['agent_id'] != b['agent_id']][:8]; print('   false merge examples', ex)
ex2 = [(a['agent_name'], b['agent_name'], a['agent_id']) for a, b in si if a['nm'] != b['nm']][:8]; print('   miss examples', ex2)
# ---- Part B
noid = [p for p in PER if p not in idy]
B = [x for x in R if x['period'] in noid and x['nm'] not in ('', '-')]
bn = collections.defaultdict(list)
for x in B: bn[x['nm']].append(x)
pb = [p for n, l in bn.items() for p in cross(l[:60])]
sb = random.sample(pb, 100)
firmsper = collections.defaultdict(set)
for x in R:
    if x['fm']: firmsper[x['nm']].add(x['fm'])
conf = single = multi = 0; lines = []
for a, b in sb:
    c = bool(a['fm'] and b['fm'] and a['fm'] != b['fm']); s = len(a['nm'].split()) == 1; m = len(firmsper[a['nm']]) > 1
    conf += c; single += s; multi += m
    lines.append(f"{a['period']}|{b['period']}|{a['agent_name']}|{a['agent_firm']}|{b['agent_firm']}|{a['registering_club']}|{b['registering_club']}|conflict={int(c)}|single={int(s)}|multi_firm={int(m)}")
print(f'B: 100 random cross-period name matches in periods without IDs: firm conflict {conf}, single-word name {single}, name linked to >1 firm in the data {multi}')
open(__import__('os').path.join(__import__('os').path.dirname(__import__('os').path.abspath(__file__)),'audit_name_matching_sample_noid.txt'), 'w', encoding='utf8').write('\n'.join(lines))
# firm-based tie-break coverage: share of no-ID rows with a firm field
print('share of rows with firm field, by period:', {p: round(sum(1 for x in R if x['period'] == p and x['fm']) / max(1, sum(1 for x in R if x['period'] == p)), 2) for p in PER})
