# app.py
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import streamlit as st
from matplotlib.colors import TwoSlopeNorm
from pathlib import Path
import seaborn as sns
import plotly.express as px
import json

# =========================
# Streamlit page settings
# =========================
st.set_page_config(
    page_title="Winter Storm Fern",
    layout="wide"
)

st.title("Winter Storm Fern")

# =========================
# Load spatial data
# =========================
@st.cache_data
def load_spatial_data():
    """
    Load county and state shapefiles.
    
    You can create these once and save them locally as:
      - data/counties.parquet
      - data/states.parquet

    Recommended workflow in Python:
      counties.to_parquet(...)
      states.to_parquet(...)
    """

    counties_sf = gpd.read_parquet("data/counties.parquet")
    states_sf = gpd.read_parquet("data/states.parquet")

    counties_sf = counties_sf.to_crs(epsg=4326)
    states_sf = states_sf.to_crs(epsg=4326)

    return counties_sf, states_sf


counties_sf, states_sf = load_spatial_data()

# =========================
# Sidebar controls
# =========================
OUTPUTS_DIR = Path("./outputs")

# aggregation_options = {
#     "Bing Tiles": "tk_facebook_pop_aggregated_bing.parquet",
#     "Administrative Regions": "tk_facebook_pop_aggregated_admin.parquet"
# }

# scatter_files = {
#     "Bing Tiles": "tk_facebook_pop_bing.parquet",
#     "Administrative Regions": "tk_facebook_pop_admin.parquet"
# }

# selected_aggregation = st.sidebar.selectbox(
#     "Type of Aggregation",
#     options=list(aggregation_options.keys())
# )

# selected_file = aggregation_options[selected_aggregation]

selected_file = "tk_facebook_pop_aggregated_bing.parquet"

# =========================
# Create the GeoJSON
# =========================

@st.cache_resource
def load_geojson():
    with open("data/counties.geojson") as f:
        return json.load(f)

county_geojson = load_geojson()
# =========================
# Load data
# =========================
@st.cache_data
def load_data(file_name):
    return pd.read_parquet(
        OUTPUTS_DIR / file_name
    )


df = load_data(selected_file)

#scatter_df = load_data(scatter_files[selected_aggregation])

scatter_df = load_data("tk_facebook_pop_bing.parquet")

df["datetime"] = (
    pd.to_datetime(
        df["ds"].astype(str) + " " + df["hour"].astype(str).str.zfill(4),
        format="%Y-%m-%d %H%M"
    )
    .dt.tz_localize("America/Los_Angeles")
    .dt.tz_convert("America/New_York")
)

scatter_df["datetime"] = (
    pd.to_datetime(
        scatter_df["ds"].astype(str) + " " + scatter_df["hour"].astype(str).str.zfill(4),
        format="%Y-%m-%d %H%M"
    )
    .dt.tz_localize("America/Los_Angeles")
    .dt.tz_convert("America/New_York")
)

counties_sf["county_geoid"] = (
    counties_sf["county_geoid"]
    .astype(str)
    .str.zfill(5)
)

df["county_geoid"] = (
    df["county_geoid"]
    .astype(str)
    .str.zfill(5)
)

date_col = "datetime"

value_col = "percent_change"


@st.cache_data
def get_map_data(df, dt):
    return df[df["datetime"] == dt]
# =========================
# Date-Time selector
# =========================

available_datetimes = sorted(df["datetime"].dropna().unique())

selected_datetime = st.sidebar.select_slider(
    "Select date and hour",
    options=available_datetimes,
    value=available_datetimes[-1],
    format_func=lambda x: x.strftime("%Y-%m-%d %I:%M %p %Z")
)

# =========================
# County selector
# =========================
available_counties = sorted(df["county_name_acs"].dropna().unique())

selected_counties = st.sidebar.multiselect(
    "Select counties",
    options=available_counties
)

# =========================
# County time series prep
# =========================

def make_county_time_data(selected_counties):

    filtered_df = df.copy()

    # Filter only if counties selected
    if len(selected_counties) > 0:
        filtered_df = filtered_df[
            filtered_df["county_name_acs"].isin(selected_counties)
        ].copy()
        filtered_df["county_name_acs"] = (
            filtered_df["county_name_acs"]
            .cat.remove_unused_categories()
        )


    return filtered_df

# =========================
# Scatterplot prep
# =========================

def create_scatter_data():

    scatter_filtered = scatter_df[
        scatter_df["datetime"] == selected_datetime
    ].copy()

    if len(selected_counties) > 0:
        scatter_filtered = scatter_filtered[
            scatter_filtered["county_name_acs"].isin(selected_counties)
        ]

    return scatter_filtered

# =========================
# Map plotting function
# =========================
# =========================
# Interactive map plotting
# =========================

def create_map(plot_datetime):

    latest_county = get_map_data(df, selected_datetime)

    fig = px.choropleth_map(
        latest_county,
        geojson=county_geojson,
        locations="county_geoid",
        featureidkey="properties.county_geoid",
        color="percent_change",
        color_continuous_scale="RdBu",
        color_continuous_midpoint=0,
        hover_name="county_name_acs",
        hover_data={
            "county_state": False,
            "percent_change": ":.2f",
            "n_crisis": ":,.0f",
            "n_baseline": ":.0f",
            "total_population": ":.0f",
            "county_geoid": False
        },
        labels={
            "county_state": "State",
            "percent_change": "Facebook Pop Change from Baseline (%)",
            "n_crisis": "Facebook Population at Given Time",
            "n_baseline": "Facebook Population at 45-day Baseline",
            "total_population": "Total Population"
        },
        map_style="carto-positron",
        center={"lat": 36, "lon": -88},
        zoom=5
    )

    fig.update_layout(
        title=plot_datetime.strftime("%Y-%m-%d %I:%M %p %Z"),
        margin={"r": 0, "t": 40, "l": 0, "b": 0},
        height=700
    )

    return fig


# =========================
# Summary
# =========================

st.markdown(
    """
    A major American winter storm, often referred to as Winter Storm Fern, started on Friday, January 23rd, 2026 and
    continued through January 26th, 2026. The storm brought heavy snow and freezing rain to several US states, ranging
    from the southern plains to the East Coast. Several states experienced network outages and extremely cold temperatures.
    For example, temperatures in Lexington, Kentucky fell to −16 degrees Fahrenheit (including wind chill) 
    ([Lexington Herald-Leader (2026)](https://www.kentucky.com/news/weather-news/article314454683.html)). Emergency declarations
    were issued in Arkansas, Georgia, Indiana, Kentucky, Louisiana, Maryland, Mississippi, North Carolina, South Carolina,
    Tennessee, Virginia, and West Virginia ([Congressional Research Service (2026) ](https://www.congress.gov/crs-product/IN12644)).
    Power outages due to downed trees and ice occurred in Southern States, such as Texas, Louisiana, Mississippi, and Tennessee.
    From Maine to New Mexico, states experienced significant snowfall and sleet. As of January 29th, there were up to 115 fatalities
    across 20 states after this winter storm, and approximately 2.5 million customers experienced power outages across the country
    ([Kothari (2026)](https://watchers.news/2026/01/29/over-100-fatalities-confirmed-after-major-january-2026-u-s-winter-storm/)).
    Verisk, who are catastrophe risk modelling specialists, estimated a total of \$4 billion in industry losses while 14 states
    could each exceed \$50 million in insured losses ([Evans (2026)](https://www.artemis.bm/news/verisk-estimates-winter-storm-fern-insured-losses-could-reach-4bn/)).
    """
)

# =========================
# Tabs
# =========================
tab_info, tab_maps, tab_scatter, tab_timeseries, tab_table, tab_background = st.tabs(
    [
        "User Information",
        "Maps",
        "Subregions Plot",
        "County Time Series",
        "Tables",
        "Background on Data"
    ]
)

# =========================
# User Information tab
# =========================
with tab_info:
    st.subheader("Interactive Situation Report")
    st.markdown(
        """
        Toggle through the "Maps", "Subregions Plot" and "County Time Series" tabs to explore the data and
        use the widgets on the sidebar to filter the data and the plots. The "Tables" tab shows the data behind
        these plots, and the "Background on Data" tab provides more information about the data processing.

        The following visualizations are developed based on Meta AI for Good's data through Facebook with a focus on
        Tennessee, Kentucky and surrounding states. The data covers population movement during the collection period
        of January 30 to February 12. These datasets use location activity from Facebook users to estimate how populations
        shift during a crisis. Data do not represent the entire population in the area. Counts of people within a certain
        region are recorded at 8-hour intervals in this dataset. The counts during the crisis are compared to the counts
        at a baseline, which are the counts in the same region 45 days prior to data collection. These regions are either Bing
        tiles, what are about 1.5 x 1.5 mile blocks, or administrative regions as determined by [GADM](https://gadm.org). These
        counts are aggregated to the county level for this analysis.
        """
    )

    video_file = open("storm_animation.mp4", "rb")
    video_bytes = video_file.read()

    st.video(video_bytes)

# =========================
# Maps tab
# =========================
with tab_maps:

    st.header("County-Level Population Change (%)")

    st.markdown(
        """
        The map below displays county-level summaries for the timestamp selected by the slider to the left. Negative changes in
        red indicate counties with an average exodus occured at the time, and positive changes in blue indicate counties counties
        with a population influx at the time.
        The subregions (i.e. Bing tiles) are aggregated up to counties.
        Each county is colored based on the percent change in Facebook population compared to the baseline, which is calculated as:
        """)
    st.latex(r'''\text{Percent Change}_{cw} =\frac{\sum_{i \in \text{Bing}_c} n_{\text{crisis},ciw}-\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}}{\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}+ 1}\times 100''')
    st.markdown(
        """
        where $c$ is the county, $w$ is the reporting window and ${Bing}_c$ denotes the set of Bing tiles assigned to county $c$
        (based on centroid location). The small value added to the denominator follows the logic in the Meta documentation and
        prevents division by very small baseline values. The plot represents county-level population density change based on aggregated counts. 
        $n_{baseline}$ is the Facebook population in
        that county during the 45-day baseline period, and $n_{crisis}$ is the Facebook population in that county during the
        selected date and hour. The color scale ranges from red (indicating a large decrease in population compared to baseline)
        to blue (indicating a large increase in population compared to baseline). Hovering over a county will show the county name,
        state, percent change, Facebook population at the given time, Facebook population at baseline, and total population according
        to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs).
        """
    )
                

    fig = create_map(selected_datetime)

    st.plotly_chart(
        fig,
        use_container_width=True
    )

# =========================
# Scatterplot tab
# =========================

with tab_scatter:

    st.header("Subregion Population Change")

    st.markdown(
         """
         The scatterplot below shows the subregion-level data points for the timestamp selected by the slider to the left. 
         All subregions (i.e. Bing Tiles) are shown by default, but you can filter for specific counties using the dropdown on the left.
         Each point represents a subregion and is colored based on the percent change in Facebook population compared to baseline,
         which is calculated as:
         """
    )
    st.latex(r'''\text{Percent Change}_{cw} =\frac{\sum_{i \in \text{Bing}_c} n_{\text{crisis},ciw}-\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}}{\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}+ 1}\times 100''')
    st.markdown(
        """
        where $c$ is the county, $w$ is the reporting window and ${Bing}_c$ denotes the set of Bing tiles assigned to county $c$
        (based on centroid location). The small value added to the denominator follows the logic in the Meta documentation and
        prevents division by very small baseline values. The plot represents county-level population density change based on aggregated counts. 
        $n_{baseline}$ is the Facebook population in
        that county during the 45-day baseline period, and $n_{crisis}$ is the Facebook population in that county during the
        selected date and hour. The color scale ranges from red (indicating a large decrease in population compared to baseline)
        to blue (indicating a large increase in population compared to baseline). Hovering over a county will show the county name,
        state, percent change, Facebook population at the given time, Facebook population at baseline, and total population according
        to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs).
        """
    )


    scatter_data = create_scatter_data()

    st.caption(f"{len(scatter_data):,} rows")

    if not scatter_data.empty:

        fig_scatter = px.scatter(
            scatter_data,
            x="longitude",
            y="latitude",
            color="percent_change",
            size="n_baseline",
            color_continuous_scale="RdBu",
            color_continuous_midpoint=0,
            hover_data={
                "latitude": False,
                "longitude": False,
                "county_name_acs": True,
                "county_state": False,
                "percent_change": ":.2f",
                "n_crisis": ":,.0f",
                "n_baseline": ":,.0f"
            },
            labels={
                "county_name_acs": "County of Subregion",
                "percent_change": "Facebook Pop Change from Baseline (%)",
                "n_crisis": "Facebook Population at Given Time",
                "n_baseline": "Facebook Population at 45-day Baseline",
                "total_population": "Total Population",
                "latitude": "Latitude",
                "longitude": "Longitude"
            }
        )

        # Add black border around markers
        fig_scatter.update_traces(
            marker=dict(
                line=dict(
                    color="black",
                    width=1
                )
            )
        )


        fig_scatter.update_layout(
            title=selected_datetime.strftime("%Y-%m-%d %I:%M %p %Z"),
            height=800,
            margin=dict(l=0, r=0, t=40, b=0)
        )



        st.plotly_chart(
            fig_scatter,
            use_container_width=True
        )

    else:
        st.warning(
            "No rows available for the selected date/hour/county filters."
        )

# =========================
# County time series tab
# =========================
with tab_timeseries:

    st.header("% Population Change Over Time")

    time_data = make_county_time_data(selected_counties)

    time_data["datetime_display"] = (
        time_data["datetime"]
        .dt.strftime("%Y-%m-%d %I:%M %p %Z")
    )

    st.markdown(
        """
        Each line in the chart below is the percent change in Facebook population compared to baseline for a county across the
        reporting windows during the data collection period. The reporting windows are 8-hour intervals that start at 3am EST,
        11am EST and 7pm EST. Note that this is not necessarily the local time since some counties are in the Central Time Zone.
        Hovering over a point will show more information about the county.
        Use the dropdown on the left to filter for specific counties. This will also display some demographic information for the
        selected counties, according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs).
        The percent change is calculated as:
        """)
    st.latex(r'''\text{Percent Change}_{cw} =\frac{\sum_{i \in \text{Bing}_c} n_{\text{crisis},ciw}-\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}}{\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}+ 1}\times 100''')
    st.markdown(
        """
        where $c$ is the county, $w$ is the reporting window and ${Bing}_c$ denotes the set of Bing tiles assigned to county $c$
        (based on centroid location). The small value added to the denominator follows the logic in the Meta documentation and
        prevents division by very small baseline values. The plot represents county-level population density change based on aggregated counts. 
        $n_{baseline}$ is the Facebook population in
        that county during the 45-day baseline period, and $n_{crisis}$ is the Facebook population in that county during the
        selected date and hour. Hovering over a county will show the county name,
        state, percent change, Facebook population at the given time, Facebook population at baseline, and total population according
        to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs).
        """
    )


    if not time_data.empty:

        # =====================================
        # User-friendly county summary metrics
        # =====================================
        if len(selected_counties) > 0:

            summary_df = (
                time_data.sort_values("datetime")
                .groupby("county_name_acs", observed=True)
                .last()
                .reset_index()
            )

            st.subheader("County Summary")

            for _, row in summary_df.iterrows():

                county_name = row["county_name_acs"]

                total_population = row.get("total_population", None)
                median_income = row.get("median_income", None)
                poverty_rate = row.get("poverty_rate", None)
                pct_age_65_plus = row.get("pct_age_65_plus", None)
                pct_no_vehicle = row.get("pct_no_vehicle", None)

                pop_text = (
                    f"{int(total_population):,}"
                    if pd.notnull(total_population)
                    else "N/A"
                )

                income_text = (
                    f"${int(median_income):,}"
                    if pd.notnull(median_income)
                    else "N/A"
                )

                poverty_text = (
                    f"{poverty_rate:.1f}%"
                    if pd.notnull(poverty_rate)
                    else "N/A"
                )

                age65_text = (
                    f"{pct_age_65_plus:.1f}%"
                    if pd.notnull(pct_age_65_plus)
                    else "N/A"
                )

                no_vehicle_text = (
                    f"{pct_no_vehicle:.1f}%"
                    if pd.notnull(pct_no_vehicle)
                    else "N/A"
                )

                st.markdown(
                    f"""
                    ### {county_name}

                    - **Total Population:** {pop_text}
                    - **Median Household Income:** {income_text}
                    - **Poverty Rate:** {poverty_text}
                    - **Age 65+ Population:** {age65_text}
                    - **Households Without Vehicle:** {no_vehicle_text}
                    """
                )

            st.divider()

        # =====================================
        # Time series chart
        # =====================================
        fig_ts = px.line(
            time_data,
            x="datetime",
            y="percent_change",
            color="county_name_acs",
            markers=True,
            hover_name="county_name_acs",
            hover_data={
                "percent_change": ":.2f",
                "datetime_display": True,
                "datetime": False
            },
            labels={
                "datetime_display": "Date and Time",
                "county_name_acs": "County Name",
                "percent_change": "Facebook Pop Change from Baseline (%)",
                "n_crisis": "Facebook Population at Given Time",
                "n_baseline": "Facebook Population at 45-day Baseline",
                "total_population": "Total Population"
            }
        )

        fig_ts.add_hline(
            y=0,
            line_dash="dash",
            line_color="black",
            line_width=1
        )

        fig_ts.update_layout(
            title="% Population Change Over Time",
            xaxis_title="",
            yaxis_title="% Change vs Baseline",
            showlegend=False,  # removes legend
            height=600
        )

        st.plotly_chart(
            fig_ts,
            use_container_width=True
        )

    else:
        st.warning("No county data available.")

# =========================
# Table tab
# =========================
with tab_table:

    st.header("Aggregated at County Level Table")

    st.markdown(
        """
        The table below shows the county-level data points for the timestamp selected by the slider to the left using the
        subregions (i.e. Bing tiles). Hover over the columns to see the definitions of each variable.
        All counties are shown by default, but you can filter for specific counties using the dropdown on the left.
        When you filter for counties, you will also see the subregion-level data table, which shows the data points for the
        subregions within the selected counties, below the county-level table.
        """
    )

    table_df = df.copy()

    table_df["datetime_display"] = (
        table_df["datetime"]
        .dt.strftime("%Y-%m-%d %I:%M %p %Z")
    )

    # Filter counties only if selected
    if len(selected_counties) > 0:
        table_df = table_df[
            table_df["county_name_acs"].isin(selected_counties)
        ]
        table_df["county_name_acs"] = (
            table_df["county_name_acs"]
            .cat.remove_unused_categories()
        )

    # Optional sorting
    table_df = table_df.sort_values(
        by=[date_col, "county_state", "county_name_acs"]
    )

    table_df["percent_change"] /= 100

    st.dataframe(
        table_df,
        use_container_width=True,
        column_order=("county_name_acs", "datetime_display", "percent_change", "n_crisis", "n_baseline", "total_population", "median_income", "poverty_rate", "pct_age_65_plus", "pct_no_vehicle"),
        column_config={
        "county_name_acs":st.column_config.Column(
            "County", 
            help = "**Definition**: County name and state"),
        "percent_change": st.column_config.NumberColumn(
            "Facebook Pop Change from Baseline (%)",
            format="percent",
            help = "**Definition**: Percent change in Facebook Location Services-enabled user population counts per tile, calculated by dividing the difference by the baseline (plus a small value, usually 1)"),
        "datetime_display": st.column_config.Column(
            "Date and Time",
            help = "**Definition**: The date and time in Eastern Time Zone"),
        "n_crisis": st.column_config.NumberColumn(
            "Facebook Population at Given Time",
            format="accounting",
            help = "**Definition**: The number of Facebook Location Services-enabled users at the given date and time"),
        "n_baseline": st.column_config.NumberColumn(
            "Facebook Population at 45-day Baseline",
            format="accounting",
            help = "**Definition**: The number of Facebook Location Services-enabled users at the 45-day baseline"),
        "total_population": st.column_config.NumberColumn(
            "Total Population",
            format="accounting",
            help = "**Definition**: The total population of the county according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs)"),
        "median_income": st.column_config.NumberColumn(
            "Median Household Income",
            format="dollar",
            help = "**Definition**: The median household income of the county  according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs)"),
        "poverty_rate": st.column_config.NumberColumn(
            "Poverty Rate",
            format="percent",
            help = "**Definition**: The percentage of the population living below the poverty line  according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs)"),
        "pct_age_65_plus": st.column_config.NumberColumn(
            "Age 65+ Population (%)",
            format="percent",
            help = "**Definition**: The percentage of the population that is 65 years or older  according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs)"),
        "pct_no_vehicle": st.column_config.NumberColumn(
            "Households Without Vehicle (%)",
            format="percent",
            help = "**Definition**: The percentage of households that do not have a vehicle  according to the [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs)")
    }
    )

    st.caption(
        f"{len(table_df):,} rows displayed"
    )

    st.divider()


        # Filter counties only if selected
    if len(selected_counties) > 0:
        st.header("Subregion Table")
        st.markdown(
            """
            The table below shows the subregion-level data points for the timestamp selected by the slider to the left using the
            subregions (i.e. Bing tiles). Hover over the columns to see the definitions of each variable.
            """
        )
        table2_df = scatter_df.copy()

        table2_df["datetime_display"] = (
            table2_df["datetime"]
            .dt.strftime("%Y-%m-%d %I:%M %p %Z")
        )

        table2_df = table2_df[
            table2_df["county_name_acs"].isin(selected_counties)
        ]
        table2_df["county_name_acs"] = (
            table2_df["county_name_acs"]
            .cat.remove_unused_categories()
        )

        # Optional sorting
        table2_df = table2_df.sort_values(
            by=[date_col, "county_state", "county_name_acs"]
        )

        table2_df["percent_change"] /= 100

        st.dataframe(
            table2_df,
            use_container_width=True,
            column_order=("county_name_acs","latitude", "longitude", "datetime_display", "percent_change", "n_crisis", "n_baseline"),
            column_config={
            "county_name_acs": st.column_config.Column(
            "County of Subregion", 
            help = "**Definition**: the corresponding county name and state of the subregion"),
            "percent_change": st.column_config.NumberColumn(
            "Facebook Pop Change from Baseline (%)",
            format="percent",
            help = "**Definition**: Percent change in Facebook Location Services-enabled user population counts per tile, calculated by dividing the difference by the baseline (plus a small value, usually 1)"),
        "datetime_display": st.column_config.Column(
            "Date and Time",
            help = "**Definition**: The date and time in Eastern Time Zone"),
        "n_crisis": st.column_config.NumberColumn(
            "Facebook Population at Given Time",
            format="accounting",
            help = "**Definition**: The number of Facebook Location Services-enabled users at the given date and time"),
        "n_baseline": st.column_config.NumberColumn(
            "Facebook Population at 45-day Baseline",
            format="accounting",
            help = "**Definition**: The number of Facebook Location Services-enabled users at the 45-day baseline"),
        "latitude": st.column_config.NumberColumn(
            "Latitude",
            format="%.1f",
            help = "**Definition**: The latitude of the centroid of the subregion (i.e. Bing tile)"),
        "longitude": st.column_config.NumberColumn(
            "Longitude",
            format="%.1f",
            help = "**Definition**: The longitude of the centroid of the subregion (i.e. Bing tile)"),
        }
        )

        st.caption(
            f"{len(table2_df):,} rows displayed"
        )


# =========================
# Background tab
# =========================
with tab_background:
    st.header("Background on Data")

    st.markdown(
        """
        The Facebook data used in this report comes from Meta’s Data for Good crisis datasets. In plain terms, these datasets use
        location activity from Facebook users or Facebook business Pages to estimate how population shifts during a crisis.
        The data does not represent everyone in the affected area: for population and movement datasets, it only represents
        Facebook app users who have Location Services enabled. For business activity, it represents qualifying Facebook Business
        Pages with enough activity to be included while preserving privacy. 
        
        The data is constructed by comparing what is observed during the crisis period to what was typical before the crisis.
        Namely, population movements during the storm is compared to a pre-crisis baseline of 45-days prior to data collection.
        Because the data is relative to a baseline, the main signal is not the raw count itself, but whether a place is above or
        below its pre-crisis level.
        
        Temporally, the population and movement datasets use fixed 8-hour windows. These windows begin at 3:00, 11:00,
        and 19:00 Eastern Time (EST). This means that the time periods do not automatically adjust to local time zones.
        Business activity is reported daily, based on the date value in the dataset. 
        Some rows have missing values for `n_baseline` or `n_crisis` because of Meta’s privacy protections around small
        counts (counts less than or eual to 10 are not reported) . To avoid dropping these rows entirely, missing `n_baseline`
        values are imputed as 3 and missing `n_crisis` values are imputed as 9. See Table 6 for percentage of data imputed.
        
        After imputation, observations are summarized by county and reporting window. Because the data may include multiple
        observations within a single day, the reporting window is defined using both the date and the time-window information
        available in the data. For each county-window pair, the baseline counts are summed across all Bing tile observations in
        that county and reporting window, and the crisis counts are summed across those same observations. The county-window
        percent change is then calculated as:
        """
    )
    st.latex(r'''\text{Percent Change}_{cw} =\frac{\sum_{i \in \text{Bing}_c} n_{\text{crisis},ciw}-\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}}{\sum_{i \in \text{Bing}_c} n_{\text{baseline},ci}+ 1}\times 100''')
    st.markdown(
        """
        where $c$ is the county, $w$ is the reporting window and ${Bing}_c$ denotes the set of Bing tiles assigned to county $c$
        (based on centroid location). The small value added to the denominator follows the logic in the Meta documentation and
        prevents division by very small baseline values. The plot represents county-level population density change based on
        aggregated counts. 

        In addition to the Meta crisis datasets, county-level demographic data was added from the
        [American Community Survey in 2022](https://www.census.gov/programs-surveys/acs). This demographic data was incorporated
        with the use of the [`tidycensus` package](https://walker-data.com/tidycensus/). This outside data provides context about
        the counties affected by the storm, including total population, median household income, poverty rate, share of residents
        age 65 and older, and share of households without vehicle access. These variables help interpret which communities may be
        more vulnerable during a crisis.
        
        The demographic data is joined using GEOID, a standardized county identifier. This is important because county names alone
        are not unique across states. For example, many states have counties with the same name, but each county has a unique GEOID.
        """
    )