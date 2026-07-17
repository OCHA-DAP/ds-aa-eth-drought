"""Export raw historical trigger data for the ETH drought MAM & JJAS windows.

Writes self-contained CSVs to ``exports/`` (see ``exports/README.md``):

  seas5_mam_perzone.csv   per-zone Feb-forecast seasonal totals, rank, RP, flag
  seas5_jjas_perzone.csv  per-zone Mar/Apr/May-forecast totals, rank, RP, flag
  trigger_history.csv     annual zone counts + backtested trigger record

Both windows are purely SEAS5-driven (from ``public.seas5`` on the PROD DB; zone
lists from the DEV blob). Faithful to analysis/05_4 (MAM) and analysis/05_5 (JJAS).

  wt1 = MAM  (Belg):   Feb-issued SEAS5, valid Mar-May,  fires at >= 15 zones @ >=5yr RP
  wt2 = JJAS (Kiremt): Apr/May-issued SEAS5, valid Jun-Sep, fires at >= 35 zones @ >=5yr RP
                       (either the Apr OR the May forecast crossing the threshold fires it;
                        the Mar forecast is exported as monitoring context only)

Per-zone return period: cumulative seasonal rainfall ranked historically per zone,
RP = (n_years + 1) / rank; a zone flags at RP >= 5 (driest ~20% of years).

Ranking pools: historical years (<= HIST_MAX_YEAR) are ranked on the historical-only
pool (n=29); the current forecast year is ranked against the historical pool + itself
and only its own counts are read off -- history is never re-ranked by the incoming
year. This matches the notebooks and is required to reproduce the endorsed list
(e.g. JJAS 2009 April = 36 zones remains flagged).
"""

import sys
from pathlib import Path

import ocha_stratus as stratus
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT))

from src.constants import days_in_month  # noqa: E402
from src.datasources.seas5 import fetch_seas5_data  # noqa: E402

EXPORT_DIR = REPO_ROOT / "exports"

START = pd.Timestamp("1997-01-01")
END = pd.Timestamp("2026-12-01")
HIST_MAX_YEAR = 2025
CURRENT_YEAR = 2026

MAM = dict(
    zones_blob="ds-aa-eth-drought/exploration/Ethiopia MAM zones.csv",
    issued_months=[2],
    season_months=[3, 4, 5],
    threshold=15,
)
JJAS = dict(
    zones_blob="ds-aa-eth-drought/exploration/Ethiopia JJAS zones.csv",
    issued_months=[3, 4, 5],  # Mar exported for context; only Apr/May are operative
    season_months=[6, 7, 8, 9],
    threshold=35,
    trigger_issued_months=[4, 5],
)


def load_seasonal_totals(cfg):
    """Per (year, zone, issue-month) cumulative seasonal rainfall (mm)."""
    zones = stratus.load_csv_from_blob(
        cfg["zones_blob"], stage="dev", container_name="projects"
    )
    pcodes = zones["adm2_pcode"].unique()
    s = fetch_seas5_data(pcodes, START, END)
    s["issued_month"] = pd.to_datetime(s["issued_date"]).dt.month
    s["valid_month"] = pd.to_datetime(s["valid_date"]).dt.month
    s["year"] = pd.to_datetime(s["valid_date"]).dt.year
    s["mean_mm"] = s["mean"] * s["valid_month"].map(days_in_month)
    seasonal = (
        s[
            s["issued_month"].isin(cfg["issued_months"])
            & s["valid_month"].isin(cfg["season_months"])
        ]
        .groupby(["year", "pcode", "issued_month"], as_index=False)["mean_mm"]
        .sum()
        .rename(columns={"mean_mm": "seasonal_total_mm"})
    )
    return seasonal, len(pcodes)


def rank_flag(seasonal):
    """Rank per (pcode, issue-month); flag zones at >=5yr RP.

    Historical years on the historical pool; CURRENT_YEAR on the combined pool.
    """

    def _rank(df):
        df = df.copy()
        df["rank"] = df.groupby(["pcode", "issued_month"])["seasonal_total_mm"].rank(
            method="min", ascending=True
        )
        n = df["year"].nunique()
        df["return_period"] = ((n + 1) / df["rank"]).round(2)
        df["flagged_ge_5yr"] = df["return_period"] >= 5
        df["n_years_ranked"] = n
        return df

    hist = _rank(seasonal[seasonal["year"] <= HIST_MAX_YEAR])
    parts = [hist]
    if (seasonal["year"] == CURRENT_YEAR).any():
        combined = _rank(seasonal[seasonal["year"] <= CURRENT_YEAR])
        parts.append(combined[combined["year"] == CURRENT_YEAR])
    return pd.concat(parts, ignore_index=True).sort_values(
        ["issued_month", "year", "return_period"], ascending=[True, True, False]
    )


def zone_counts(perzone, prefix):
    """Wide table: year x issue-month zone counts at >=5yr RP, cols <prefix>_<mon>."""
    import calendar

    c = (
        perzone[perzone["flagged_ge_5yr"]]
        .groupby(["year", "issued_month"])
        .size()
        .reset_index(name="n")
        .pivot(index="year", columns="issued_month", values="n")
        .reindex(sorted(perzone["year"].unique()))
        .fillna(0)
        .astype(int)
    )
    c.columns = [f"{prefix}_{calendar.month_abbr[m].lower()}_zones" for m in c.columns]
    return c.reset_index()


def main():
    EXPORT_DIR.mkdir(exist_ok=True)

    # --- MAM ---
    mam_seasonal, n_mam = load_seasonal_totals(MAM)
    mam_pz = rank_flag(mam_seasonal)
    mam_pz.to_csv(EXPORT_DIR / "seas5_mam_perzone.csv", index=False)
    mam_c = zone_counts(mam_pz, "mam")  # -> mam_feb_zones
    mam_c["mam_triggered"] = mam_c["mam_feb_zones"] >= MAM["threshold"]

    # --- JJAS ---
    jjas_seasonal, n_jjas = load_seasonal_totals(JJAS)
    jjas_pz = rank_flag(jjas_seasonal)
    jjas_pz.to_csv(EXPORT_DIR / "seas5_jjas_perzone.csv", index=False)
    jjas_c = zone_counts(jjas_pz, "jjas")  # -> jjas_mar/apr/may_zones
    op_cols = [
        f"jjas_{m}_zones"
        for m in ("apr", "may")
        if f"jjas_{m}_zones" in jjas_c.columns
    ]
    jjas_c["jjas_triggered"] = jjas_c[op_cols].ge(JJAS["threshold"]).any(axis=1)

    # --- annual trigger history ---
    hist = mam_c.merge(jjas_c, on="year", how="outer").sort_values("year")
    hist["eth_drought_v1_wt1"] = hist["mam_triggered"]  # MAM (window A)
    hist["eth_drought_v1_wt2"] = hist["jjas_triggered"]  # JJAS (window B)
    cols = [
        "year",
        "mam_feb_zones",
        "mam_triggered",
        "jjas_mar_zones",
        "jjas_apr_zones",
        "jjas_may_zones",
        "jjas_triggered",
        "eth_drought_v1_wt1",
        "eth_drought_v1_wt2",
    ]
    hist = hist[[c for c in cols if c in hist.columns]]
    hist.to_csv(EXPORT_DIR / "trigger_history.csv", index=False)

    print(f"MAM zones: {n_mam}   JJAS zones: {n_jjas}")
    print(hist.to_string(index=False))
    print(f"\nwrote 3 files -> {EXPORT_DIR}")
    return hist


if __name__ == "__main__":
    main()
