# CMU AI-SDM + Meta Repository

## Repo Structure

* [EDA Code](https://github.com/selinacarter/AISDM-Meta-Repo/tree/main/EDA_Code)
* [Interactive Version](https://github.com/selinacarter/AISDM-Meta-Repo/tree/main/Interactive_Version) - (Python )An interactive streamlit version of situation reports for disasters, including:
  * Winter Storm Fern - 02/2026
  * California Wildfires - 05/2026
* [Metadata JSONs](https://github.com/selinacarter/AISDM-Meta-Repo/tree/main/Metadata_JSONS)
* [SAM Attempt](https://github.com/selinacarter/AISDM-Meta-Repo/tree/main/SAM_Attempt) - (Python) Initial attempts at incorporating Meta's SAM model for disaster management
* [Situation Reports](https://github.com/selinacarter/AISDM-Meta-Repo/tree/main/Situation_Reports) - (R) Brief documents that are designed to help inform emergency managers (EMs) on people movements after each disaster, including:
  * Winter Storm Fern - 02/2026 **FINISHED**
  * California Wildfires - 05/2026
  * Bordeaux Wildfires - 07/2026
  * Spokane Fires - 08/2026 **FINISHED**
  * Colombia Earthquakes - 08/2026 **FINISHED**

## Folder Structure: EDA Code

The files in this folder are used for data-cleaning and initial exploratory data analysis.
Note that the code for this file is largely intended to be disaster-agnostic, except for the
files named as `3_[disaster]_functions.R`, which are intended to be generic for particular disaster types.

* `1_data_cleaning.R` has code that consolidates all .csv files for the population and movement datasets.
* `2_plot_functions.R` has generic plotting functions for population during crisis and displacement.
* `3_[disaster]_functions.R` files are for adding disaster-specific information onto the above plots, including
  * `3_fire_functions` for plotting wildfires polygons
  * `3_earthquakes_functions` for earthquakes as points colored by magnitude
* `4_animations.R` creates time-lapsed animations of the population during crisis or displacement plots
* `eda.qmd` sources the above code files and can be run for and adjusted for preliminary data analysis.

## Folder Structure: Interactive Version

We have also developed a digital version of the situation report using Streamlit. This app allows for interactive maps and customizable plots that can be filtered for specific counties and/or time zones. This app utilizes the same datasets as the situation report: Meta’s Data for Good crisis datasets, which are limited-assess datasets that track population changes through Facebook.

Each folder has the following files: \* `prepare_spatial_data.py` which prepares county-level spatial data and saves them as .parquet files \* `make_animation.py` which creates an .mp4 file that dynamically visualizes population movement over time (this appears on the first page of the interactive version) \* `display.py` which is the Streamlit app and runs locally using the following terminal command: `streamlit run display.py`

The above .py files must be run in the above order (i.e. prepare spatial data first, generate the animation, and then run the app). A demo of this interactive version for the Winter Storm Fern report can be found [here](https://www.youtube.com/watch?v=b2er7hXYK1Y).

## Folder Structure: Metadata JSONs

The details of each disaster analysis is summarized in a metadata JSON file that follows [this template](https://github.com/selinacarter/AISDM-Meta-Repo/blob/main/Metadata_JSONS/template.JSON). 
By running the [`validate_metadata.R`](https://github.com/selinacarter/AISDM-Meta-Repo/blob/main/Metadata_JSONS/validate_metadata.R) file, the [README file](https://github.com/selinacarter/AISDM-Meta-Repo/blob/main/Metadata_JSONS/README.md)
in this folder updates with a tabular summary for all disasters that have been analyzed, high-level information about the disaster,
and current stage of development.

## Folder Structure: SAM Attempt

We are in the process of developing a workflow that utilizes [Meta's SAM model](https://ai.meta.com/research/sam3/), possibly in conjunction with Meta’s Data for Good crisis datasets. The most up-to-date information can be found in the [README](https://github.com/nikita-jaya/AISDM-Meta-Repo/blob/main/SAM%20Attempt/README.md) for this folder.

Note: we have yet to integrate it with Meta's Data for Good crisis datasets. We have been experimenting with images from the [CRASAR-U-DROIDs dataset](https://huggingface.co/datasets/CRASAR/CRASAR-U-DROIDs).

## Folder Structure: Situation Reports

The structure of these reports is based on the [CrisisReady Situation Report for LA Fires](https://www.crisisready.io/resources/situation-reports/). The situation report utilizes Meta’s Data for Good crisis datasets, which are limited-assess datasets that track population changes through Facebook.

Each folder *always* has following files: \* `data_cleaning.R` or equivalent to clean the Meta datasets and append additional information, such as demographics or Bing tile polygons \* `situation-report.qmd` / `situation-report.pdf` which are the code and report, respectively, for the final deliverables

Occasionally, the folder also has a subset of the following files: \* `references.bib` for additional sources used in the prose or graphics (for the latter, this is usually for tracking the location of the disaster) \* `.mp4` or `.gif` files, which are dynamic visualizations of movement (these are used for initial EDA or the interaction version, if relevant) \* `Previous Work/` folder, which has EDA files and/or previous iterations of the report. These are separate from the version-controlled commits on the final copy because the saved iterations of the report were often submitted for feedback.

## Data

Since Meta's datasets are limited-access, the datasets are saved in Google Drive and are not publicly available.
