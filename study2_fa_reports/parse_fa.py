"""Extract the FA's published intermediary / football-agent transaction reports (raw/<period>/fa.bin) into one long table.

Usage: python3 -I parse_fa.py            (run in this folder; needs pdftotext)
Writes: fa_transactions_long.csv  one row per transaction x intermediary (sides collapsed to flags)
        fa_transactions_wide_early.csv  the early reports as published (one row per transaction)
        fa_unparsed_lines.csv  lines the parser could not place, for audit
"""
import re, subprocess, csv, json, sys, collections
from pathlib import Path
ROOT = Path(__file__).parent
RAW = ROOT / "raw"

PERIODS = {  # period: (family, start, end as stated in the report title or FA news page, published date on the report)
    "2015-16": ("A", "2015-04-01", "2016-02-01", "2016-04-22"),
    "2016-17": ("A", "2016-02-02", "2017-01-31", ""),
    "2017-18": ("A", "2017-02-01", "2018-01-31", "2018-03-27"),
    "2018-19": ("A", "2018-02-01", "2019-01-31", "2019-03-31"),
    "2019-20": ("B", "2019-02-01", "2020-01-31", "2020-06-24"),   # period from the FA news page; the PDF carries no title
    "2020-21": ("C", "2020-02-01", "2021-02-01", "2021-03-31"),
    "2021-22": ("D", "", "", "2022-03-31"),                        # PDF carries no title or header text
    "2022-23": ("D", "", "", "2023-03-31"),
    "2023-24": ("E", "2023-02-01", "2024-02-01", "2024-04-12"),
    "2024-25": ("E", "2024-02-02", "2025-02-03", "2025-04-14"),
    "2025-26": ("E", "2025-02-04", "2026-02-02", "2026-04-01"),
}
# columns by family (list of field names in order); the sub flag (YES/NO) is the last cell in B-E
FIELDS = {
    "B": ["date", "club", "player", "former_club", "reg_type", "agent", "agent_id", "representing", "subcontractor"],
    "C": ["club", "player", "former_club", "reg_type", "agent", "agent_id", "representing", "subcontractor"],
    "D": ["club", "player", "reg_type", "agent", "agent_id", "representing", "subcontractor"],
    "E": ["club", "player", "reg_type", "agent", "representing", "subcontractor"],
}
A_FIELDS = [("date", r"Date Received"), ("club", r"Registering Club"), ("player", r"Player"), ("p_im", r"Player IM"), ("p_org", r"Player IM Organisation"),
            ("c_im", r"Registering Club IM"), ("c_org", r"Registering Club IM Organisation"), ("f_im", r"Former Club IM"), ("f_org", r"Former Club IM Organisation"),
            ("former_club", r"Former Club")]
SECTION = re.compile(r"^(Premier League|EFL|Championship|League (One|Two)|National League|Women|FA Women|Barclays|Sky Bet|Football League|WSL)", re.I)
DATE = re.compile(r"^\s*\d{2}/\d{2}/\d{2,4}\b")
FLAG = re.compile(r"\s(YES|NO)\s*$", re.I)


VARIANTS = {   # column sets that occur within a family (lower-league pages omit some columns), keyed by number of cells
    "B": {9: FIELDS["B"], 7: ["date", "club", "player", "agent", "agent_id", "representing", "subcontractor"]},
    "C": {8: FIELDS["C"], 6: ["club", "player", "agent", "agent_id", "representing", "subcontractor"]},
    "D": {7: FIELDS["D"], 6: ["club", "player", "agent", "agent_id", "representing", "subcontractor"]},
    "E": {6: FIELDS["E"]},
}


def cells(line):
    """Tokens separated by two or more spaces, with their start columns."""
    return [(m.start(), m.group().strip()) for m in re.finditer(r"\S(?:\S| (?! ))*", line)]


def assign(tokens, starts):
    out = [""] * len(starts)
    for pos, txt in tokens:
        k = max([i for i, s in enumerate(starts) if s <= pos + 2] or [0])
        out[k] = (out[k] + " " + txt).strip()
    return out


def pdf_text(period):
    return subprocess.check_output(["pdftotext", "-layout", str(RAW / period / "fa.bin"), "-"], text=True).split("\f")


def header_starts_A(line):
    # labels are separated by 2+ spaces; match in order of position
    toks = cells(line)
    names = [t for _, t in toks]
    mapping = {lab: pos for pos, lab in toks}
    starts = []
    for key, lab in A_FIELDS:
        if lab not in mapping:
            return None
        starts.append(mapping[lab])
    return starts


def parse_A(period, unparsed):
    recs = []
    starts = None; section = ""
    for pg, text in enumerate(pdf_text(period), 1):
        for ln in text.split("\n"):
            if "Date Received" in ln:
                s = header_starts_A(ln)
                if s: starts = s
                continue
            if not ln.strip() or starts is None:
                continue
            toks = cells(ln)
            if not DATE.match(ln):
                if re.search(r"Intermediary Transactions|information|hacking|FIFA|guarantee", ln) or re.fullmatch(r"\s*\d{2}/\d{2}/\d{4}\s*", ln):
                    continue
                if len(toks) == 1 and toks[0][0] <= starts[1] + 2 and not re.search(r"\d", ln):
                    section = toks[0][1] if SECTION.match(toks[0][1]) else section; continue
            c = [("" if v.strip() == "-" else v) for v in assign(toks, starts)]
            if DATE.match(ln):
                r = dict(zip([k for k, _ in A_FIELDS], c)); r.update(page=pg, raw=ln.strip(), wrapped=False, section=section)
                recs.append(r)
            elif recs and recs[-1]["page"] == pg:
                r = recs[-1]
                for (k, _), v in zip(A_FIELDS, c):
                    if v: r[k] = (r[k] + " " + v).strip()
                r["wrapped"] = True; r["raw"] += " || " + ln.strip()
            else:
                unparsed.append((period, pg, ln.strip()))
    return recs


def split_slash(s):
    return [x.strip() for x in s.split(" / ")] if s else []


def long_from_A(period, recs):
    rows = []
    for r in recs:
        base = dict(period=period, date=r["date"], registering_club=r["club"], former_club=r["former_club"], player=r["player"], reg_type="",
                    source_page=r["page"], raw_text=r["raw"], section=r.get("section", ""))
        agents = {}   # (name, org) -> set of sides
        flags = []
        for side, im, org in (("player", "p_im", "p_org"), ("registering_club", "c_im", "c_org"), ("former_club", "f_im", "f_org")):
            names, orgs = split_slash(r[im]), split_slash(r[org])
            if names and orgs and len(names) != len(orgs):
                flags.append(f"{side}: {len(names)} individuals vs {len(orgs)} organisations")
                orgs = orgs + [""] * (len(names) - len(orgs)) if len(orgs) < len(names) else orgs
            if not names and any(orgs):
                names = [""] * len(orgs)
                flags.append(f"{side}: organisation without individual")
            for i, n in enumerate(names):
                o = orgs[i] if i < len(orgs) else ""
                agents.setdefault((n, o), set()).add(side)
        if not agents:
            flags.append("no intermediary cell")
        for (n, o), sides in agents.items():
            rows.append(dict(base, agent_name=n or o, agent_individual=n, agent_firm=o, agent_id="", representing="; ".join(sorted(sides)),
                             side_player="player" in sides, side_registering_club="registering_club" in sides, side_former_club="former_club" in sides,
                             side_coach=False, subcontractor="", flags="; ".join(flags + (["wrapped line merged"] if r["wrapped"] else []))))
    return rows


def parse_flagged(period, fam, unparsed):
    """Families B-E: lines ending in the YES/NO subcontractor flag; column starts are taken per page from complete rows."""
    out, last_starts, section = [], None, ""
    for pg, text in enumerate(pdf_text(period), 1):
        lines = [re.sub(r"^(\s*\d{2}/\d{2}/\d{2,4}) ", r"\1   ", l) if fam == "B" else l for l in text.split("\n")]
        counts = collections.Counter(len(cells(l)) for l in lines if FLAG.search(l) and len(cells(l)) in VARIANTS[fam])
        if counts:
            n = counts.most_common(1)[0][0]; fields = VARIANTS[fam][n]
            full = [cells(l) for l in lines if FLAG.search(l) and len(cells(l)) == n]
            starts = [sorted(t[i][0] for t in full)[len(full) // 2] for i in range(n)]
        elif last_starts:
            fields, starts = last_fields, last_starts      # page without a complete row: reuse the previous page's columns
            n = len(fields)
        else:
            for l in lines:
                if l.strip() and FLAG.search(l): unparsed.append((period, pg, l.strip()))
            continue
        last_starts, last_fields = starts, fields
        for l in lines:
            if not l.strip(): continue
            if FLAG.search(l):
                toks = cells(l)
                c = assign(toks, starts) if len(toks) != n else [t for _, t in toks]
                r = dict(zip(fields, c)); r.update(page=pg, raw=l.strip(), wrapped=False, blank_cells=len(toks) != n, section=section)
                out.append(r)
            else:
                # no flag cell: header, title, date line, or a wrapped fragment
                if len(cells(l)) == 1 and len(l.split()) <= 5 and not re.search(r"IMS|[a-z]\s{3,}", l.strip()):
                    section = l.strip() if SECTION.match(l.strip()) else section; continue   # league heading
                if re.search(r"Registering Club|Representing|Subcontractor|Intermediary|information|Football Agent|Transactions|^\s*\d{2}/\d{2}/\d{4}\s*$|FIFA|guarantee|The FA", l): continue
                unparsed.append((period, pg, l.strip()))
    return out


# Wrapped rows whose cells are split across lines; each was checked against the source page (same five as in the earlier extract, taken over after re-checking)
REPAIRS = {
    "2021-22": [(5, "Manchester City", "Adedire Emmanuel Oluwatofunmi Iyiola Awokoya-Mebude", "New Registration", "David Moss", "IMS003179", "Player,Club"),
                (11, "Hull City", "Ryan James Longman", "Perm Transfer In", "DIRK HEBEL SPORTS CONSULTING KRONEN 2696 GmbH & Co Vorrats KG", "IMSC001420", "Player,Club")],
    "2022-23": [(8, "West Ham United", "Jan Thilo Kehrer", "Perm Transfer In", "DIRK HEBEL SPORTS CONSULTING KRONEN 2696 GmbH & Co Vorrats KG", "IMSC001420", "Club,Player")],
    "2025-26": [(27, "Blackpool", "Oluchukwu Joshua Chukwu Amaka Aka Chukwu Nzube Chukwu Nwankwo", "New Registration", "Adnan Ahmed", "", "Player")],
}


def repaired(period, fam):
    out = []
    for pg, club, player, typ, agent, aid, rep in REPAIRS.get(period, []):
        r = dict(club=club, player=player, reg_type=typ, agent=agent, agent_id=aid, representing=rep, subcontractor="NO", page=pg,
                 raw="[manual repair of a wrapped row]", wrapped=True, blank_cells=False)
        out.append(r)
    return out


def long_from_flagged(period, fam, recs):
    rows = []
    for r in recs:
        rep = (r.get("representing") or "").replace(" ", "").lower()
        sides = set(x for x in rep.split(",") if x)
        aid = r.get("agent_id", "")
        kind = "firm" if aid.startswith("IMSC") else ("individual" if aid.startswith("IMS") else "")
        name = r["agent"]
        ind, firm = (name if kind == "individual" else ""), (name if kind == "firm" else "")
        compound = ""
        if " - " in name and name.count(" - ") == 1:
            a, b = [x.strip() for x in name.split(" - ")]
            isfirm = lambda z: bool(re.search(r"\b(Ltd|Limited|Management|Sports?|Group|LLP|Agency|Consult\w*|Football|Soccer|Services|S\.?L\.?|GmbH|Associates|Representation|Solicitors|Law|UK)\b", z, re.I))
            if isfirm(a) != isfirm(b): (firm, ind) = (a, b) if isfirm(a) else (b, a); compound = "compound name split into individual and firm"
        rows.append(dict(period=period, date=r.get("date", ""), registering_club=r["club"], former_club=r.get("former_club", ""), player=r["player"],
                         reg_type=r.get("reg_type", ""), source_page=r["page"], raw_text=r["raw"], section=r.get("section", ""), agent_name=name,
                         agent_individual=ind, agent_firm=firm, agent_id=aid,
                         representing=r.get("representing", ""), side_player="player" in sides, side_registering_club=bool({"club", "registeringclub"} & sides),
                         side_former_club="formerclub" in sides, side_coach="coach" in sides, subcontractor=(r.get("subcontractor") or "").upper(),
                         flags=("manual wrap repair; " if r["wrapped"] else "") + ("blank cell(s) placed by position" if r["blank_cells"] else "")))
    return rows


def main():
    unparsed, long_rows, wide = [], [], []
    for period, (fam, *_ ) in PERIODS.items():
        if fam == "A":
            recs = parse_A(period, unparsed); wide += [dict(period=period, **{k: v for k, v in r.items()}) for r in recs]
            long_rows += long_from_A(period, recs)
        else:
            recs = [r for r in parse_flagged(period, fam, unparsed) if r.get("club") or r.get("player")] + repaired(period, fam); long_rows += long_from_flagged(period, fam, recs)
        print(period, fam, "records", len(recs), "long rows so far", len(long_rows), file=sys.stderr)
    cols = ["period", "date", "registering_club", "former_club", "player", "reg_type", "agent_name", "agent_individual", "agent_firm", "agent_id", "representing",
            "side_player", "side_registering_club", "side_former_club", "side_coach", "subcontractor", "section", "source_page", "flags", "raw_text"]
    with open(ROOT / "fa_transactions_long.csv", "w", newline="", encoding="utf8") as f:
        w = csv.DictWriter(f, fieldnames=cols); w.writeheader(); w.writerows(long_rows)
    if wide:
        with open(ROOT / "fa_transactions_wide_early.csv", "w", newline="", encoding="utf8") as f:
            w = csv.DictWriter(f, fieldnames=list(wide[0].keys())); w.writeheader(); w.writerows(wide)
    with open(ROOT / "fa_unparsed_lines.csv", "w", newline="", encoding="utf8") as f:
        w = csv.writer(f); w.writerow(["period", "page", "line"]); w.writerows(unparsed)
    json.dump({p: list(v) for p, v in PERIODS.items()}, open(ROOT / "periods.json", "w"), indent=1)


main()
