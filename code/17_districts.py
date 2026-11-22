"""State house districts on one fixed map per storm, for origin and destination addresses,
and each district's two-party presidential vote share (exploratory, added 2026-11-17).

Fixed map = the NC House districts recorded in the storm cohort's baseline snapshot
(May 2018 for Florence, May 2024 for Helene). A census block is assigned the modal district of
the baseline voters geocoded to it; blocks without baseline voters take the modal district of
their block group, then tract. Origin and destination addresses of every cohort are assigned a
district through their block, so redistricting never counts as a move.

District vote share: precinct-sorted presidential results (2016 for Florence, 2020 for Helene;
NCSBE adds statistical noise to small cells) allocated to districts in proportion to the
baseline voters of each precinct registered in each district.

Usage: py -I code/17_districts.py setup <event>   -> block map, district vote shares
       py -I code/17_districts.py dest  <event>   -> PROC/dist_<event>_<baseline>.parquet (pid, hd_o, hd_d)
Aggregates written to output/tables: district_votes_<event>.csv, district_diag_<event>.csv
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, PROC, OUT

EV = {"florence": dict(b="20180508", year=2016,
                       cohorts=[("20150101", "20160607"), ("20170101", "20180508"), ("20180508", "20190910")]),
      "helene": dict(b="20240514", year=2020,
                     cohorts=[("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"),
                              ("20240514", "20250909")])}
stage, ev = sys.argv[1], sys.argv[2]
cfg = EV[ev]
TAB = OUT / "tables"
con = duckdb.connect()
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
coh = lambda b: f"read_parquet('{(PROC / f'cohort_{ev}_{b}.parquet').as_posix()}')"
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
geo = lambda d: f"read_parquet('{(INTER / 'geocode' / f'geo_{d}.parquet').as_posix()}')"
bmap = PROC / f"blockmap_{ev}.parquet"
HD = "nullif(trim(NC_house_abbrv),'')"

if stage == "setup":
    b = cfg["b"]
    con.execute(f"""CREATE TABLE v AS SELECT geoid, {HD} AS hd, county_id, trim(precinct_abbrv) AS pct
                    FROM {coh(b)} WHERE {HD} IS NOT NULL""")
    con.execute("""CREATE TABLE g AS SELECT geoid, hd FROM v WHERE geoid IS NOT NULL""")
    for name, n in (("blk", 15), ("bg", 12), ("tr", 11)):
        con.execute(f"""CREATE TABLE m_{name} AS SELECT k, hd, n, tot FROM (
            SELECT substr(geoid,1,{n}) k, hd, count(*) n, sum(count(*)) OVER (PARTITION BY substr(geoid,1,{n})) tot,
                   row_number() OVER (PARTITION BY substr(geoid,1,{n}) ORDER BY count(*) DESC, hd) rn
            FROM g GROUP BY 1,2) WHERE rn = 1""")
    con.execute(f"""COPY (SELECT 'blk' lvl, k, hd FROM m_blk UNION ALL SELECT 'bg', k, hd FROM m_bg
                          UNION ALL SELECT 'tr', k, hd FROM m_tr) TO '{bmap.as_posix()}' (FORMAT parquet)""")
    pur = con.execute("SELECT count(*) blocks, sum(tot) voters, 100.0*sum(n)/sum(tot) pct_in_modal, "
                      "100.0*avg((n=tot)::INT) pct_blocks_pure FROM m_blk").df()
    pur.insert(0, "item", "block to district"); print(pur.to_string())
    # precinct results
    if cfg["year"] == 2016:
        f = (RAW / "elections" / "results_sort_20161108.txt").as_posix()
        res = f"""SELECT county_id::VARCHAR county_id, trim(precinct_code) pct,
                    sum(CASE WHEN candidate_name ILIKE '%trump%' THEN votes ELSE 0 END) r,
                    sum(CASE WHEN candidate_name ILIKE '%clinton%' THEN votes ELSE 0 END) d
                  FROM read_csv('{f}', delim='\t', header=true, ignore_errors=true)
                  WHERE contest_name = 'US PRESIDENT' GROUP BY 1,2"""
    else:
        f = (RAW / "elections" / "sort_2020.txt").as_posix()
        res = f"""SELECT county_id::VARCHAR county_id, trim(precinct_code) pct,
                    sum(CASE WHEN trim(candidate_party_lbl)='REP' THEN vote_ct ELSE 0 END) r,
                    sum(CASE WHEN trim(candidate_party_lbl)='DEM' THEN vote_ct ELSE 0 END) d
                  FROM read_csv('{f}', delim='\t', header=true, ignore_errors=true, quote='')
                  WHERE trim(contest_title) = 'US PRESIDENT' GROUP BY 1,2"""
    con.execute(f"CREATE TABLE res AS {res}")
    con.execute("""CREATE TABLE w AS SELECT county_id, pct, hd, count(*)::DOUBLE / sum(count(*)) OVER (PARTITION BY county_id, pct) w,
                     count(*) n FROM v GROUP BY 1,2,3""")
    cov = con.execute("""SELECT (SELECT sum(r+d) FROM res) votes_all,
        (SELECT sum(r+d) FROM res WHERE (county_id, pct) IN (SELECT (county_id, pct) FROM w)) votes_matched,
        (SELECT sum(n) FROM w) voters_all,
        (SELECT sum(n) FROM w WHERE (county_id, pct) IN (SELECT (county_id, pct) FROM res)) voters_matched""").df()
    cov["pct_votes_matched"] = 100 * cov.votes_matched / cov.votes_all
    cov["pct_voters_matched"] = 100 * cov.voters_matched / cov.voters_all
    print(cov.to_string())
    # Precinct labels that do not match (precincts renamed or redrawn between the election and
    # the baseline): the county's unmatched votes are allocated to districts in proportion to the
    # county's baseline voters in unmatched precincts.
    dv = con.execute("""
      WITH a AS (SELECT w.hd, w.w*res.r r, w.w*res.d d FROM w JOIN res USING (county_id, pct)),
      ru AS (SELECT county_id, sum(r) r, sum(d) d FROM res
             WHERE (county_id, pct) NOT IN (SELECT (county_id, pct) FROM w) GROUP BY 1),
      wu AS (SELECT county_id, hd, sum(n)::DOUBLE / sum(sum(n)) OVER (PARTITION BY county_id) w FROM w
             WHERE (county_id, pct) NOT IN (SELECT (county_id, pct) FROM res) GROUP BY 1,2),
      b AS (SELECT wu.hd, wu.w*ru.r r, wu.w*ru.d d FROM wu JOIN ru USING (county_id))
      SELECT hd, sum(r) r, sum(d) d, sum(r)/sum(r+d) rep2 FROM (SELECT * FROM a UNION ALL SELECT * FROM b)
      GROUP BY 1 ORDER BY 1""").df()
    nv = con.execute("SELECT hd, count(*) voters, count(DISTINCT county_id) counties FROM v GROUP BY 1").df()
    dv = dv.merge(nv, on="hd", how="outer"); dv.insert(0, "event", ev)
    dv.to_csv(TAB / f"district_votes_{ev}.csv", index=False)
    s = dv.rep2
    print("districts:", len(dv), "missing share:", int(s.isna().sum()),
          "| within 5:", int(((s - .5).abs() < .05).sum()), "within 10:", int(((s - .5).abs() < .10).sum()),
          "| statewide rep2 from districts: %.4f" % (dv.r.sum() / (dv.r.sum() + dv.d.sum())))
    pd.concat([pur, cov.assign(item="precinct match")]).to_csv(TAB / f"district_diag_{ev}.csv", index=False)

elif stage == "dest":
    con.execute(f"CREATE TABLE bm AS SELECT * FROM read_parquet('{bmap.as_posix()}')")
    con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
    look = lambda g: f"""coalesce((SELECT hd FROM bm WHERE lvl='blk' AND k = {g}),
                                  (SELECT hd FROM bm WHERE lvl='bg' AND k = substr({g},1,12)),
                                  (SELECT hd FROM bm WHERE lvl='tr' AND k = substr({g},1,11)))"""
    rows = []
    for b, f in cfg["cohorts"]:
        # follow-up record of each NCID (same rule as 06_build_cohort.py) and its geocoded block
        con.execute(f"""CREATE OR REPLACE TABLE fu AS
          SELECT p.pid, x.county_id, g.geoid FROM (
            SELECT ncid, county_id, voter_reg_num,
                   row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
            FROM {snap(f)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') x
          JOIN pid p ON p.ncid = x.ncid
          LEFT JOIN (SELECT county_id, voter_reg_num, geoid FROM {geo(f)} WHERE tier <> 'unmatched' AND NOT ambiguous) g
            ON g.county_id = x.county_id AND g.voter_reg_num = x.voter_reg_num
          WHERE x.rn = 1""")
        out = PROC / f"dist_{ev}_{b}.parquet"
        con.execute(f"""COPY (
          SELECT c.pid, {look('c.geoid')} AS hd_o,
                 CASE WHEN c.addr_{f} THEN {look('fu.geoid')} END AS hd_d
          FROM (SELECT pid, CASE WHEN tier <> 'unmatched' AND NOT ambiguous THEN geoid END AS geoid, addr_{f}
                FROM {coh(b)}) c LEFT JOIN fu ON fu.pid = c.pid) TO '{out.as_posix()}' (FORMAT parquet)""")
        r = con.execute(f"""SELECT '{b}' baseline, '{f}' followup, count(*) n,
              100.0*avg((d.hd_o IS NOT NULL)::INT) pct_origin_district,
              sum(c.addr_{f}::INT) n_addr_change,
              100.0*sum((c.addr_{f} AND d.hd_d IS NOT NULL)::INT)/sum(c.addr_{f}::INT) pct_dest_district,
              100.0*sum((c.addr_{f} AND c.dhouse_{f} IS NOT NULL)::INT)/sum(c.addr_{f}::INT) pct_dest_old_lookup
            FROM {coh(b)} c JOIN read_parquet('{out.as_posix()}') d USING (pid)""").df()
        print(r.to_string(), flush=True); rows.append(r)
    pd.concat(rows).assign(event=ev).to_csv(TAB / f"district_dest_diag_{ev}.csv", index=False)
