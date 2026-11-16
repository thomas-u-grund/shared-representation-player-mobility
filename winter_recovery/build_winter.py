"""Link known pre-January player profiles to January transfers and candidate clubs."""
from pathlib import Path
import argparse
from collections import Counter, defaultdict
import re
import unicodedata
import zipfile
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument('source_folder', type=Path, help='Directory of Kaggle players.csv download bodies named data_VERSION')
parser.add_argument('transfer_folder', type=Path, help='Directory of transfer and game files')
args = parser.parse_args()
sources = pd.read_csv('sources.csv', dtype={'version':str})
leagues = set('BE1 DK1 ES1 FR1 GB1 GR1 IT1 L1 NL1 PO1 RU1 SC1 TR1 UKR1'.split())

def key(value):
    return re.sub('[^a-z0-9]', '', unicodedata.normalize('NFKD', value.casefold()).encode('ascii','ignore').decode())

g = pd.read_csv(args.transfer_folder/'games.gz', usecols=['season','competition_id','home_club_id','away_club_id'])
members = defaultdict(dict)
for game in g[g.competition_id.isin(leagues)].itertuples():
    for c in (game.home_club_id,game.away_club_id):
        members[game.season][c] = game.competition_id
raw = pd.read_csv(args.transfer_folder/'transfers.gz', low_memory=False)
raw = raw.dropna(subset=['from_club_id','to_club_id','transfer_date'])
raw = raw[raw.from_club_id.ne(raw.to_club_id)].copy()
raw['positive_fee'] = raw.transfer_fee.fillna(0).gt(0)
e = raw.groupby(['player_id','transfer_date','from_club_id','to_club_id'],as_index=False).positive_fee.max()
history = e.groupby(['player_id','from_club_id','to_club_id']).transfer_date.agg(list).to_dict()

snapshots = []
arrivals = []
candidates = []
counts = []
for source in sources.itertuples():
    year = source.january
    assert source.file_last_modified[:10] < f'{year}-01-01'
    f = args.source_folder / ('data_' + source.version)
    p = pd.read_csv(f, compression='zip' if zipfile.is_zipfile(f) else None,low_memory=False)
    clubs = members[year-1]
    roster = p[p.current_club_id.isin(clubs) & p.last_season.eq(year-1)].copy()
    assert not roster.duplicated(['player_id','current_club_id']).any()
    roster['agent_label'] = roster.agent_name.fillna('').str.strip()
    missing = {'','null','none','-','nan','n/a','unknown','without agent','no agent'}
    roster.loc[roster.agent_label.str.lower().isin(missing),'agent_label'] = ''
    roster['agent_key'] = roster.agent_label.map(key)
    roster['league'] = roster.current_club_id.map(clubs)
    roster['january'] = year
    roster['snapshot_version'] = source.version
    roster['snapshot_date'] = source.file_last_modified[:10]
    roster = roster.rename(columns={'current_club_id':'club_id','name':'player_name'})
    snapshots.append(roster[['january','snapshot_version','snapshot_date','player_id','player_name','club_id','league','agent_label','agent_key']])
    z = e[e.transfer_date.between(f'{year}-01-01',f'{year}-01-31') &
          e.from_club_id.isin(clubs) & e.to_club_id.isin(clubs)].copy()
    z['origin_league'] = z.from_club_id.map(clubs)
    z['destination_league'] = z.to_club_id.map(clubs)
    z['january'] = year
    z['event_id'] = [f'W{year}-{i:04}' for i in range(1, len(z)+1)]
    z = z.merge(roster[['player_id','club_id','agent_label','agent_key']],how='left',
                left_on=['player_id','from_club_id'],right_on=['player_id','club_id'],validate='many_to_one')
    z['origin_snapshot_match'] = z.club_id.notna()
    z[['agent_label','agent_key']] = z[['agent_label','agent_key']].fillna('')
    z['reverse_move_in_prior_year'] = [
        any(f'{year-1}-01-01' <= d < row.transfer_date for d in history.get((row.player_id,row.to_club_id,row.from_club_id),()))
        for row in z.itertuples()]
    z['zero_or_unknown_fee_reverse'] = ~z.positive_fee & z.reverse_move_in_prior_year
    labelled = roster[roster.agent_key.ne('')]
    roster_size = Counter(labelled.club_id)
    co_clients = Counter(zip(labelled.club_id,labelled.agent_key))
    player_club_labels = {(r.player_id,r.club_id):r.agent_key for r in labelled.itertuples()}
    for row in z[z.agent_key.ne('')].itertuples():
        pool = sorted(c for c,l in clubs.items() if l == row.destination_league and c != row.from_club_id and roster_size[c])
        if row.to_club_id not in pool or len(pool)<2:
            continue
        for c in pool:
            focal = player_club_labels.get((row.player_id,c)) == row.agent_key
            candidates.append(dict(event_id=row.event_id,january=year,candidate_club=c,chosen=int(c==row.to_club_id),
                                   known_roster_labels=roster_size[c],co_clients=co_clients[c,row.agent_key]-int(focal)))
    arrivals.append(z.drop(columns='club_id'))
    counts.append(dict(january=year,clubs=len(clubs),roster_players=len(roster),named_roster_players=len(labelled),
                       january_moves=len(z),between_league=int(z.origin_league.ne(z.destination_league).sum()),
                       origin_matches=int(z.origin_snapshot_match.sum()),named_movers=int(z.agent_key.ne('').sum()),
                       named_excluding_zero_or_unknown_fee_reversals=int((z.agent_key.ne('')&~z.zero_or_unknown_fee_reverse).sum()),
                       eligible_choices=len({c['event_id'] for c in candidates if c['january']==year})))
pd.concat(snapshots,ignore_index=True).to_csv('winter_player_snapshots.csv',index=False)
pd.concat(arrivals,ignore_index=True).to_csv('january_transfers.csv',index=False)
c = pd.DataFrame(candidates)
assert not c.duplicated(['event_id','candidate_club']).any() and c.groupby('event_id').chosen.sum().eq(1).all()
c.to_csv('january_destination_candidates.csv',index=False)
pd.DataFrame(counts).to_csv('coverage.csv',index=False)
print(pd.DataFrame(counts).to_string(index=False))
