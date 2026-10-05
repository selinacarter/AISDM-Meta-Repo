import geopandas as gpd
from pygris import counties, states
from pathlib import Path
import pandas as pd

# Counties
counties_sf = counties(
    cb=True,
    year=2023
).to_crs(4326)[
    ["STATE_NAME", "NAME", "GEOID", "geometry"]
]

counties_sf = counties_sf.rename(columns={
    "STATE_NAME": "county_state",
    "NAME": "county_name",
    "GEOID": "county_geoid"
})

# States
states_sf = states(
    cb=True,
    year=2023
).to_crs(4326)[
    ["NAME", "geometry"]
]

states_sf = states_sf.rename(columns={
    "NAME": "state_name"
})

# Save
counties_sf.to_parquet("data/counties.parquet")
states_sf.to_parquet("data/states.parquet")
counties_sf[["county_geoid", "geometry"]].to_file(
    "data/counties.geojson",
    driver="GeoJSON"
)

# convert csv to parquet

# prepare_parquet.py


INPUT_DIR = Path("outputs")
OUTPUT_DIR = Path("outputs")

OUTPUT_DIR.mkdir(exist_ok=True)

FILES = [
    "tk_facebook_pop_aggregated_bing.csv",
    "tk_facebook_pop_aggregated_admin.csv",
    "tk_facebook_pop_bing.csv",
    "tk_facebook_pop_admin.csv",
]

# Columns actually used by the Streamlit app
REQUIRED_COLUMNS = [
    "county_geoid",
    "county_name_acs",
    "county_state",
    "percent_change",
    "ds",
    "hour",

    # scatter plot fields
    "latitude",
    "longitude",
    "n_crisis",
    "n_baseline",

    # county summary fields
    "total_population",
    "median_income",
    "poverty_rate",
    "pct_age_65_plus",
    "pct_no_vehicle",
]

for file_name in FILES:

    print(f"Processing {file_name}")

    df = pd.read_csv(INPUT_DIR / file_name)

    # Keep only columns that exist in this file
    cols = [c for c in REQUIRED_COLUMNS if c in df.columns]
    df = df[cols]

    # Optional dtype optimization
    if "county_geoid" in df.columns:
        df["county_geoid"] = (
            df["county_geoid"]
            .astype(str)
            .str.zfill(5)
        )

    for col in [
        "percent_change",
        "latitude",
        "longitude",
        "poverty_rate",
        "pct_age_65_plus",
        "pct_no_vehicle",
    ]:
        if col in df.columns:
            df[col] = df[col].astype("float32")

    for col in [
        "n_crisis",
        "n_baseline",
        "total_population",
        "median_income",
        "hour",
    ]:
        if col in df.columns:
            df[col] = pd.to_numeric(
                df[col],
                downcast="integer"
            )

    for col in [
        "county_name_acs",
        "county_state",
    ]:
        if col in df.columns:
            df[col] = df[col].astype("category")

    output_file = (
        OUTPUT_DIR
        / file_name.replace(".csv", ".parquet")
    )

    df.to_parquet(
        output_file,
        engine="pyarrow",
        compression="snappy",
        index=False
    )

    print(
        f"Saved {output_file} "
        f"({len(df):,} rows)"
    )