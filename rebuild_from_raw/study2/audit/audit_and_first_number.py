"""Audit of the extracted FA reports and the first number: how often does the player-side intermediary on an incoming signing
already have a documented earlier dealing with the registering club (on the club's side, or on a player's side only)?

Usage: python3 -I audit_and_first_number.py     (needs pandas; run after parse_fa.py)
Writes: audit_by_period.csv, audit_club_names.csv, first_number_by_period.csv, first_number_examples.csv
Exposure is strictly pre-period: history for a focal record comes only from reporting periods that ended before the focal period began
(variant 'within' also uses earlier-dated rows in the same period where the report gives dates).
"""
import re, unicodedata
import pandas as pd
from pathlib import Path
HERE = Path(__file__).parent
ROOT = HERE.parent   # rebuild_from_raw/study2/
DATA = ROOT.parent.parent / "data" / "study2"
D = pd.read_csv(DATA / "fa_transactions_long.csv", dtype=str, keep_default_na=False)
PERIODS = ["2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "2025-26"]
ORD = {p: i for i, p in enumerate(PERIODS)}
D["ord"] = D.period.map(ORD)
for c in ["side_player", "side_registering_club", "side_former_club", "side_coach"]:
    D[c] = D[c].eq("True")


def norm(s, club=False):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().lower()
    s = re.sub(r"\(.*?\)", " ", s)
    s = re.sub(r"[^a-z0-9& ]+", " ", s)
    stop = {"ltd", "limited", "llp", "plc", "the", "uk", "sl", "gmbh"} | ({"fc", "afc", "cf", "a", "f", "c"} if club else set())
    t = [w for w in s.split() if w not in stop]
    return " ".join(t)


def player_key(s):
    t = norm(s).split()
    return (t[0] + " " + t[-1]) if t else ""


D["club_k"] = D.registering_club.map(lambda x: norm(x, True))
D["player_k"] = D.player.map(player_key)
D["agent_keys"] = [frozenset(k for k in (norm(a), norm(b), norm(c)) if k and k != "-") for a, b, c in zip(D.agent_name, D.agent_individual, D.agent_firm)]
D["focal_key"] = D.agent_name.map(norm)
D["date_p"] = pd.to_datetime(D.date, format="mixed", dayfirst=True, errors="coerce")
RENEW = re.compile(r"updated|new contract|contract amend|renew", re.I)
D["renewal"] = D.reg_type.str.contains(RENEW)
SIGN_STRICT = {"permanent transfer", "perm transfer in"}
SIGN_BROAD = SIGN_STRICT | {"new registration"}
D["signing_strict"] = D.reg_type.str.lower().isin(SIGN_STRICT)
D["signing_broad"] = D.reg_type.str.lower().isin(SIGN_BROAD)

# ---------------------------------------------------------------- audit by period
rows = []
unp = pd.read_csv(ROOT / "fa_unparsed_lines.csv", dtype=str, keep_default_na=False)
for p in PERIODS:
    x = D[D.period == p]
    tx = x.drop_duplicates(["registering_club", "player", "date", "reg_type"])
    rows.append(dict(period=p, rows=len(x), transactions=len(tx), clubs=x.club_k.nunique(), players=x.player_k.nunique(), agents=x.focal_key.nunique(),
                     share_date=(x.date != "").mean(), share_type=(x.reg_type != "").mean(), share_former_club=(x.former_club != "").mean(),
                     share_agent_id=(x.agent_id != "").mean(), share_firm_named=(x.agent_firm != "").mean(), share_side_given=(x.representing != "").mean(),
                     share_player_side=x.side_player.mean(), share_club_side=x.side_registering_club.mean(), share_subcontractor_flag=(x.subcontractor != "").mean(),
                     share_subcontractor_yes=(x.subcontractor == "YES").mean(), rows_flagged=(x["flags"] != "").sum(), manual_repairs=x["flags"].str.contains("manual wrap").sum(),
                     unparsed_lines=int((unp.period == p).sum())))
A = pd.DataFrame(rows)
A.round(3).to_csv(HERE / "audit_by_period.csv", index=False)

# name consistency across periods: normalised club key present in the previous period?
cl = []
for p in PERIODS[1:]:
    cur, prev = set(D[D.period == p].club_k), set(D[D.period == PERIODS[ORD[p] - 1]].club_k)
    allprev = set(D[D.ord < ORD[p]].club_k)
    cl.append(dict(period=p, clubs=len(cur), in_previous_period=len(cur & prev), in_any_earlier_period=len(cur & allprev), not_seen_before=len(cur - allprev),
                   examples_not_seen_before="; ".join(sorted(cur - allprev)[:12])))
pd.DataFrame(cl).to_csv(HERE / "audit_club_names.csv", index=False)

# ---------------------------------------------------------------- first number
def history_flags(f, hist):
    """f: focal rows of one period; hist: all rows from earlier periods. Returns per focal row whether a *different* player's record exists
    for the same club and an agent name in common, on the club side or only on the player side."""
    club_side, player_only, renew_side, club_only = {}, {}, {}, {}
    for r in hist.itertuples():
        for k in r.agent_keys:
            key = (r.club_k, k)
            club_side.setdefault(key, set()), player_only.setdefault(key, set()), renew_side.setdefault(key, set()), club_only.setdefault(key, set())
            if r.side_registering_club:
                club_side[key].add(r.player_k)
                if r.renewal: renew_side[key].add(r.player_k)
                if not r.side_player: club_only[key].add(r.player_k)
            elif r.side_player:
                player_only[key].add(r.player_k)
    out = []
    for r in f.itertuples():
        keys = [(r.club_k, k) for k in r.agent_keys]
        cs = any(club_side.get(k, set()) - {r.player_k} for k in keys)
        rn = any(renew_side.get(k, set()) - {r.player_k} for k in keys)
        po = any(player_only.get(k, set()) - {r.player_k} for k in keys)
        co = any(club_only.get(k, set()) - {r.player_k} for k in keys)
        out.append((cs, rn, po, co))
    return out


res, ex = [], []
for p in PERIODS[4:]:   # periods with a reg_type column
    cur = D[(D.period == p)]
    hist = D[D.ord < ORD[p]]
    for defn, col in (("strict: permanent transfer in", "signing_strict"), ("broad: + new registration", "signing_broad")):
        f = cur[cur[col] & cur.side_player].drop_duplicates(["club_k", "player_k", "focal_key"])
        if not len(f): continue
        fl = history_flags(f, hist)
        cs = sum(a for a, _, _, _ in fl); rn = sum(b for _, b, _, _ in fl); po = sum((not a) and c for a, _, c, _ in fl); co = sum(d for _, _, _, d in fl)
        both_sides_now = f.side_registering_club.sum()
        # variant using earlier-dated rows of the same period (only where the report carries dates)
        within = ""
        if (cur.date != "").mean() > 0.9:
            w = 0
            for r in f.itertuples():
                h = cur[(cur.date_p < r.date_p) & (cur.club_k == r.club_k) & cur.side_registering_club & (cur.player_k != r.player_k)]
                w += any(set(a) & set(r.agent_keys) for a in h.agent_keys)
            within = int(w)
        res.append(dict(period=p, definition=defn, history_periods=ORD[p], history_from=PERIODS[0], focal_signings=len(f),
                        agent_on_club_side_before=cs, share_club_side=round(cs / len(f), 3), of_which_via_renewal=rn, club_only_before=co, share_club_only=round(co / len(f), 3),
                        player_side_only_before=po, share_player_only=round(po / len(f), 3), no_earlier_dealing=len(f) - cs - po,
                        focal_deal_also_club_side=int(both_sides_now), club_side_extra_from_same_period_dates=within))
        if defn.startswith("strict") and p in ("2019-20", "2023-24"):
            for r, (a, b, c, d) in zip(f.itertuples(), fl):
                if a: ex.append(dict(period=p, club=r.registering_club, player=r.player, agent=r.agent_name))
R = pd.DataFrame(res)
R.to_csv(HERE / "first_number_by_period.csv", index=False)
pd.DataFrame(ex).head(60).to_csv(HERE / "first_number_examples.csv", index=False)
print(A.round(3).to_string())
print(pd.DataFrame(cl).drop(columns="examples_not_seen_before").to_string())
print(R.to_string())
