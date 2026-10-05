import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import imageio.v2 as imageio
from pathlib import Path
import json
import time
import numpy as np

from matplotlib.colors import Normalize, LinearSegmentedColormap

OUTPUTS_DIR = Path("./outputs")
FRAME_DIR = Path("./frames")
FRAME_DIR.mkdir(exist_ok=True)

# -------------------------
# Load data
# -------------------------
df = pd.read_parquet(
    OUTPUTS_DIR / "tk_facebook_pop_aggregated_bing.parquet"
)

with open("data/counties.geojson") as f:
    county_geojson = json.load(f)

counties = gpd.read_file("data/counties.geojson")

# Ensure consistent key types
df["county_geoid"] = df["county_geoid"].astype(str)
counties["county_geoid"] = counties["county_geoid"].astype(str)

# -------------------------
# Create datetime column
# -------------------------
df["datetime"] = (
    pd.to_datetime(
        df["ds"].astype(str) + " " + df["hour"].astype(str).str.zfill(4),
        format="%Y-%m-%d %H%M"
    )
    .dt.tz_localize("America/Los_Angeles")
    .dt.tz_convert("America/New_York")
)

times = sorted(df["datetime"].unique())

# -------------------------
# Static geometry
# -------------------------
base_geo = counties[["county_geoid", "geometry"]]

# -------------------------
# Color scale
# -------------------------
vmin = df["percent_change"].min()
vmax = df["percent_change"].max()

print(f"vmin = {vmin}")
print(f"vmax = {vmax}")

if vmin >= 0 or vmax <= 0:
    raise ValueError(
        "Data must contain both negative and positive values "
        "for a red-white-blue diverging scale."
    )

# Position of zero on the colorbar
white_position = (0 - vmin) / (vmax - vmin)

print(f"white position = {white_position:.3f}")

# Custom colormap:
# vmin -> dark red
# 0 -> white
# vmax -> dark blue
cmap = LinearSegmentedColormap.from_list(
    "red_white_blue_unbalanced",
    [
        (0.0, "#b2182b"),             # dark red
        (white_position, "#ffffff"),  # white at zero
        (1.0, "#2166ac"),             # dark blue
    ]
)

norm = Normalize(vmin=vmin, vmax=vmax)

# -------------------------
# Frame generation
# -------------------------
for i, dt in enumerate(times):

    start_time = time.time()
    print(f"Processing frame {i+1}/{len(times)}: {dt}")

    current = df[df["datetime"] == dt]

    merged = base_geo.merge(
        current,
        on="county_geoid",
        how="left"
    )

    fig, ax = plt.subplots(figsize=(16, 9))

    merged.plot(
        column="percent_change",
        cmap=cmap,
        norm=norm,
        linewidth=0.1,
        edgecolor="black",
        legend=False,
        ax=ax,
        missing_kwds={"color": "lightgrey"}
    )

    # -------------------------
    # Custom colorbar
    # -------------------------
    sm = plt.cm.ScalarMappable(
        cmap=cmap,
        norm=norm
    )
    sm.set_array([])

    cbar = fig.colorbar(
        sm,
        ax=ax,
        shrink=0.75,
        pad=0.01
    )

    cbar.set_label(
        "Percent Change",
        fontsize=12
    )


    # Create ticks every 10 units
    tick_start = 20 * np.floor(vmin / 20)
    tick_end = 20 * np.ceil(vmax / 20)

    ticks = np.arange(
        tick_start,
        tick_end + 20,
        20
    )

    cbar.set_ticks(ticks)

    # -------------------------
    # Fixed viewport
    # -------------------------
    ax.set_xlim(-95, -75)
    ax.set_ylim(30, 50)

    ax.set_axis_off()
    ax.set_title(
        dt.strftime("%Y-%m-%d %I:%M %p %Z"),
        fontsize=14
    )

    frame_path = FRAME_DIR / f"frame_{i:04d}.png"

    plt.savefig(
        frame_path,
        dpi=128,
        bbox_inches="tight"
    )

    plt.close()

    end_time = time.time()
    print(
        f"Frame {i+1} done in "
        f"{end_time - start_time:.2f} seconds"
    )

# -------------------------
# Create video
# -------------------------
frames = sorted(FRAME_DIR.glob("*.png"))

with imageio.get_writer(
    "storm_animation.mp4",
    fps=5
) as writer:

    for frame in frames:
        writer.append_data(
            imageio.imread(frame)
        )

print("Video saved: storm_animation.mp4")