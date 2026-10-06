library(shiny)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(scales)
library(htmltools)
library(stringr)

source("R/data.R")


# ============================================================
# Static Data
# ============================================================

APP_DATA <- load_static_data()


# ============================================================
# Percent Change Color Settings
#
# County:
#   Uses its own observed positive maximum, rounded upward
#
# Bing:
#   Positive values are capped at 1000% for color/legend only
#
# Both:
#   negative = red
#   zero     = white
#   positive = blue
# ============================================================

COUNTY_POSITIVE_CAP <- 100
BING_POSITIVE_CAP <- 500


# ============================================================
# Nice Rounded Positive Endpoint
# ============================================================

nice_endpoint <- function(x) {
  
  x <- abs(x)
  
  if (
    !is.finite(x) ||
    x <= 0
  ) {
    return(1)
  }
  
  breaks <- pretty(
    c(0, x),
    n = 5
  )
  
  max(
    breaks,
    na.rm = TRUE
  )
}


# ============================================================
# Nice Rounded Negative Endpoint
# ============================================================

make_negative_limit <- function(values) {
  
  negative_values <- values[
    is.finite(values) &
      values < 0
  ]
  
  if (length(negative_values) == 0) {
    return(-20)
  }
  
  observed <- abs(
    min(
      negative_values,
      na.rm = TRUE
    )
  )
  
  step <- dplyr::case_when(
    observed <= 50 ~ 10,
    observed <= 100 ~ 20,
    observed <= 250 ~ 50,
    observed <= 500 ~ 100,
    TRUE ~ 200
  )
  
  -ceiling(
    observed / step
  ) * step
}


# ============================================================
# County Scale
#
# County remains around its observed range, so if the maximum
# is approximately 320%, the legend will end around a clean
# value such as 350% or 400%.
# ============================================================

make_county_percent_scale <- function(values) {
  
  finite_values <- values[
    is.finite(values)
  ]
  
  positive_values <- finite_values[
    finite_values > 0
  ]
  
  positive_limit <- COUNTY_POSITIVE_CAP
  
  list(
    negative_limit =
      make_negative_limit(values),
    
    positive_limit =
      positive_limit,
    
    negative_share =
      0.20
  )
}


# ============================================================
# Bing Scale
#
# Bing retains the wider 1000%+ scale.
# ============================================================

make_bing_percent_scale <- function(values) {
  
  list(
    negative_limit =
      make_negative_limit(values),
    
    positive_limit =
      BING_POSITIVE_CAP,
    
    negative_share =
      0.15
  )
}


COUNTY_PERCENT_SCALE <- make_county_percent_scale(
  APP_DATA$county_df$percent_change
)


BING_PERCENT_SCALE <- make_bing_percent_scale(
  APP_DATA$scatter_df$percent_change
)


# ============================================================
# Convert Actual Percent Change to Gradient Position
#
# The negative portion and positive portion are continuous,
# with zero positioned exactly at white.
# ============================================================

percent_position <- function(
    x,
    scale_info
) {
  
  negative_limit <-
    scale_info$negative_limit
  
  positive_limit <-
    scale_info$positive_limit
  
  negative_share <-
    scale_info$negative_share
  
  
  # ----------------------------------------------------------
  # Cap values for coloring only
  # ----------------------------------------------------------
  
  x_capped <- pmax(
    x,
    negative_limit
  )
  
  x_capped <- pmin(
    x_capped,
    positive_limit
  )
  
  
  position <- rep(
    NA_real_,
    length(x_capped)
  )
  
  
  # ----------------------------------------------------------
  # Negative side
  # ----------------------------------------------------------
  
  negative_index <-
    !is.na(x_capped) &
    x_capped < 0
  
  
  position[
    negative_index
  ] <-
    negative_share *
    (
      (
        x_capped[
          negative_index
        ] -
          negative_limit
      ) /
        abs(
          negative_limit
        )
    )
  
  
  # ----------------------------------------------------------
  # Zero
  # ----------------------------------------------------------
  
  zero_index <-
    !is.na(x_capped) &
    x_capped == 0
  
  
  position[
    zero_index
  ] <-
    negative_share
  
  
  # ----------------------------------------------------------
  # Positive side
  # ----------------------------------------------------------
  
  positive_index <-
    !is.na(x_capped) &
    x_capped > 0
  
  
  position[
    positive_index
  ] <-
    negative_share +
    (
      1 -
        negative_share
    ) *
    (
      x_capped[
        positive_index
      ] /
        positive_limit
    )
  
  
  position
}


# ============================================================
# Continuous Color Function
#
# Dark Red -> White -> Dark Blue
# ============================================================

percent_colors <- function(
    x,
    scale_info
) {
  
  negative_share <-
    scale_info$negative_share
  
  
  color_anchors <- c(
    "#67001F",
    "#B2182B",
    "#D6604D",
    "#F4A582",
    "#FDDBC7",
    "#FFFFFF",
    "#EAF3F8",
    "#D1E5F0",
    "#92C5DE",
    "#4393C3",
    "#2166AC",
    "#053061"
  )
  
  
  color_positions <- c(
    
    seq(
      0,
      negative_share,
      length.out = 6
    ),
    
    seq(
      negative_share,
      1,
      length.out = 7
    )[-1]
  )
  
  
  palette_function <- scales::gradient_n_pal(
    colours = color_anchors,
    values = color_positions
  )
  
  
  positions <- percent_position(
    x,
    scale_info
  )
  
  
  colors <- rep(
    "transparent",
    length(positions)
  )
  
  
  valid <- is.finite(
    positions
  )
  
  
  colors[
    valid
  ] <- palette_function(
    positions[
      valid
    ]
  )
  
  
  colors
}


# ============================================================
# Continuous HTML Legend
#
# County:
#   smaller / normal-sized legend
#
# Bing:
#   taller legend so 100% increments have more spacing
# ============================================================

add_percent_legend <- function(
    map,
    scale_info,
    position = "bottomright",
    negative_step = 20,
    positive_step = 100,
    legend_height = 360,
    top_plus = FALSE
) {
  
  negative_limit <-
    scale_info$negative_limit
  
  positive_limit <-
    scale_info$positive_limit
  
  negative_share <-
    scale_info$negative_share
  
  
  # ----------------------------------------------------------
  # Negative breaks
  # ----------------------------------------------------------
  
  negative_breaks <- seq(
    0,
    negative_limit,
    by = -negative_step
  )
  
  
  if (
    tail(
      negative_breaks,
      1
    ) != negative_limit
  ) {
    
    negative_breaks <- c(
      negative_breaks,
      negative_limit
    )
  }
  
  
  # ----------------------------------------------------------
  # Positive breaks
  # ----------------------------------------------------------
  
  positive_breaks <- seq(
    0,
    positive_limit,
    by = positive_step
  )
  
  
  if (
    tail(
      positive_breaks,
      1
    ) != positive_limit
  ) {
    
    positive_breaks <- c(
      positive_breaks,
      positive_limit
    )
  }
  
  
  # ----------------------------------------------------------
  # Combined legend values
  # ----------------------------------------------------------
  
  legend_values <- sort(
    unique(
      c(
        negative_breaks[
          negative_breaks < 0
        ],
        0,
        positive_breaks[
          positive_breaks > 0
        ]
      )
    )
  )
  
  
  # ----------------------------------------------------------
  # Position labels according to the same continuous scale
  # used for map colors
  # ----------------------------------------------------------
  
  visual_positions <- percent_position(
    legend_values,
    scale_info
  )
  
  
  label_top <- 100 *
    (
      1 -
        visual_positions
    )
  
  
  # ----------------------------------------------------------
  # Legend labels
  # ----------------------------------------------------------
  
  legend_labels <- paste0(
    scales::comma(
      legend_values
    ),
    "%"
  )
  
  
  if (top_plus) {
    
    legend_labels[
      legend_values ==
        positive_limit
    ] <- paste0(
      scales::comma(
        positive_limit
      ),
      "%+"
    )
  }
  
  
  # ----------------------------------------------------------
  # Tick + Label HTML
  # ----------------------------------------------------------
  
  label_html <- paste0(
    
    sprintf(
      
      paste0(
        
        "<div style='",
        "position:absolute;",
        "top:%.4f%%;",
        "left:30px;",
        "width:8px;",
        "height:1px;",
        "background:#555;",
        "'></div>",
        
        "<div style='",
        "position:absolute;",
        "top:%.4f%%;",
        "left:43px;",
        "transform:translateY(-50%%);",
        "white-space:nowrap;",
        "font-size:13px;",
        "color:#444;",
        "'>%s</div>"
      ),
      
      label_top,
      label_top,
      legend_labels
    ),
    
    collapse = ""
  )
  
  
  # ----------------------------------------------------------
  # Continuous Gradient
  # ----------------------------------------------------------
  
  zero_percent <-
    negative_share * 100
  
  
  gradient_css <- paste0(
    
    "linear-gradient(to top, ",
    
    "#67001F 0%, ",
    
    "#B2182B ",
    round(
      zero_percent * 0.25,
      2
    ),
    "%, ",
    
    "#D6604D ",
    round(
      zero_percent * 0.50,
      2
    ),
    "%, ",
    
    "#F4A582 ",
    round(
      zero_percent * 0.70,
      2
    ),
    "%, ",
    
    "#FDDBC7 ",
    round(
      zero_percent * 0.88,
      2
    ),
    "%, ",
    
    "#FFFFFF ",
    round(
      zero_percent,
      2
    ),
    "%, ",
    
    "#D1E5F0 ",
    round(
      zero_percent +
        (
          100 -
            zero_percent
        ) * 0.20,
      2
    ),
    "%, ",
    
    "#92C5DE ",
    round(
      zero_percent +
        (
          100 -
            zero_percent
        ) * 0.40,
      2
    ),
    "%, ",
    
    "#4393C3 ",
    round(
      zero_percent +
        (
          100 -
            zero_percent
        ) * 0.65,
      2
    ),
    "%, ",
    
    "#2166AC ",
    round(
      zero_percent +
        (
          100 -
            zero_percent
        ) * 0.85,
      2
    ),
    "%, ",
    
    "#053061 100%)"
  )
  
  
  # ----------------------------------------------------------
  # Legend HTML
  # ----------------------------------------------------------
  
  legend_html <- HTML(
    
    paste0(
      
      "<div style='",
      "background:rgba(255,255,255,0.93);",
      "padding:10px 14px 12px 14px;",
      "border-radius:8px;",
      "box-shadow:0 1px 5px rgba(0,0,0,0.35);",
      "'>",
      
      
      "<div style='",
      "font-weight:bold;",
      "font-size:16px;",
      "margin-bottom:10px;",
      "'>",
      "Percent Change",
      "</div>",
      
      
      "<div style='",
      "position:relative;",
      "height:",
      legend_height,
      "px;",
      "width:115px;",
      "'>",
      
      
      "<div style='",
      "position:absolute;",
      "left:0;",
      "top:0;",
      "width:28px;",
      "height:",
      legend_height,
      "px;",
      "border:1px solid #555;",
      "background:",
      gradient_css,
      ";",
      "'></div>",
      
      
      label_html,
      
      
      "</div>",
      
      "</div>"
    )
  )
  
  
  leaflet::addControl(
    map,
    html = legend_html,
    position = position
  )
}


# ============================================================
# Formatting Helpers
# ============================================================

format_number <- function(x) {
  
  ifelse(
    is.na(x),
    "N/A",
    scales::comma(
      round(x)
    )
  )
}


format_percent_value <- function(x) {
  
  ifelse(
    is.na(x),
    "N/A",
    paste0(
      scales::comma(
        round(
          x,
          1
        )
      ),
      "%"
    )
  )
}


format_time_choice <- function(x) {
  
  format(
    x,
    "%I:%M %p",
    tz = "America/New_York"
  )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      
      HTML(
        "
        body {
          font-family: Arial, Helvetica, sans-serif;
        }

        h1, h2, h3, h4 {
          color: #003366;
        }

        .nav-tabs > li > a {
          color: #003366;
          font-weight: 600;
        }

        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          background-color: #003366;
          color: white;
        }

        .control-panel {
          background-color: #f7f9fb;
          border: 1px solid #d9e1e8;
          border-radius: 8px;
          padding: 14px;
          margin-bottom: 16px;
        }

        .info-box {
          background-color: #f7f9fb;
          border-left: 5px solid #003366;
          padding: 15px 18px;
          margin-bottom: 15px;
        }

        .storm-animation {
          display: block;
          width: 600px;
          max-width: 70%;
          height: auto;
          margin: 20px auto 25px auto;
          border-radius: 6px;
        }

        .tile-note {
          background-color: #fff8dc;
          border-left: 5px solid #d8a700;
          padding: 12px 15px;
          margin-bottom: 15px;
        }
        "
      )
    )
  ),
  
  
  titlePanel(
    "Winter Storm Fern"
  ),
  
  
  # ==========================================================
  # Storm Introduction
  # ==========================================================
  
  div(
    
    class = "info-box",
    
    HTML(
      
      paste0(
        
        "A major American winter storm, often referred to as Winter Storm Fern, ",
        "started on Friday, January 23rd, 2026 and continued through January 26th, 2026. ",
        
        "The storm brought heavy snow and freezing rain to several US states, ranging ",
        "from the southern plains to the East Coast. Several states experienced network ",
        "outages and extremely cold temperatures. For example, temperatures in Lexington, ",
        "Kentucky fell to −16 degrees Fahrenheit (including wind chill) ",
        
        "(<a href='https://www.kentucky.com/news/weather-news/article314454683.html' ",
        "target='_blank'>Lexington Herald-Leader (2026)</a>). ",
        
        "Emergency declarations were issued in Arkansas, Georgia, Indiana, Kentucky, ",
        "Louisiana, Maryland, Mississippi, North Carolina, South Carolina, Tennessee, ",
        "Virginia, and West Virginia ",
        
        "(<a href='https://www.congress.gov/crs-product/IN12644' ",
        "target='_blank'>Congressional Research Service (2026)</a>). ",
        
        "Power outages due to downed trees and ice occurred in Southern States, such as ",
        "Texas, Louisiana, Mississippi, and Tennessee. From Maine to New Mexico, states ",
        "experienced significant snowfall and sleet. As of January 29th, there were up ",
        "to 115 fatalities across 20 states after this winter storm, and approximately ",
        "2.5 million customers experienced power outages across the country ",
        
        "(<a href='https://watchers.news/2026/01/29/over-100-fatalities-confirmed-after-major-january-2026-u-s-winter-storm/' ",
        "target='_blank'>Kothari (2026)</a>). ",
        
        "Verisk, who are catastrophe risk modelling specialists, estimated a total of ",
        "$4 billion in industry losses while 14 states could each exceed $50 million ",
        "in insured losses ",
        
        "(<a href='https://www.artemis.bm/news/verisk-estimates-winter-storm-fern-insured-losses-could-reach-4bn/' ",
        "target='_blank'>Evans (2026)</a>)."
      )
    )
  ),
  
  
  # ==========================================================
  # Tabs
  # ==========================================================
  
  tabsetPanel(
    
    id = "main_tabs",
    
    
    # ========================================================
    # User Information
    # ========================================================
    
    tabPanel(
      
      "User Information",
      
      br(),
      
      h3(
        "About the Data"
      ),
      
      p(
        paste0(
          "The Facebook Population During Crisis dataset estimates ",
          "the number of Facebook users present within geographic ",
          "areas during a crisis relative to a baseline period."
        )
      ),
      
      p(
        paste0(
          "The Bing Tile Map uses the original Bing quadkey to ",
          "reconstruct the true Bing tile polygons."
        )
      ),
      
      tags$video(
        class = "storm-animation",
        src = "storm_animation.mp4",
        controls = NA,
        autoplay = NA,
        muted = NA,
        loop = NA,
        playsinline = NA
      )
    ),
    
    
    # ========================================================
    # County Map
    # ========================================================
    
    tabPanel(
      
      "County Map",
      
      br(),
      
      fluidRow(
        
        column(
          
          3,
          
          div(
            
            class = "control-panel",
            
            dateInput(
              
              "county_date",
              
              "Date:",
              
              value =
                min(
                  APP_DATA$available_dates
                ),
              
              min =
                min(
                  APP_DATA$available_dates
                ),
              
              max =
                max(
                  APP_DATA$available_dates
                )
            ),
            
            selectInput(
              "county_hour",
              "Time:",
              choices = NULL
            )
          )
        ),
        
        
        column(
          
          9,
          
          leafletOutput(
            "county_map",
            height = 620
          )
        )
      )
    ),
    
    
    # ========================================================
    # Bing Tile Map
    # ========================================================
    
    tabPanel(
      
      "Bing Tile Map",
      
      br(),
      
      div(
        
        class = "tile-note",
        
        HTML(
          paste0(
            "<b>Tile geometry:</b> Tile polygons are constructed ",
            "directly from Bing quadkeys and are 1.5 by 1.5 miles in area."
          )
        )
      ),
      
      fluidRow(
        
        column(
          
          3,
          
          div(
            
            class = "control-panel",
            
            dateInput(
              
              "scatter_date",
              
              "Date:",
              
              value =
                min(
                  APP_DATA$scatter_dates
                ),
              
              min =
                min(
                  APP_DATA$scatter_dates
                ),
              
              max =
                max(
                  APP_DATA$scatter_dates
                )
            ),
            
            selectInput(
              "scatter_hour",
              "Time:",
              choices = NULL
            ),
            
            selectizeInput(
              
              "scatter_counties",
              
              "Counties:",
              
              choices =
                APP_DATA$scatter_counties,
              
              multiple = TRUE,
              
              options =
                list(
                  placeholder =
                    "All counties"
                )
            )
          )
        ),
        
        
        column(
          
          9,
          
          leafletOutput(
            "bing_map",
            height = 620
          )
        )
      )
    ),
    
    
    # ========================================================
    # County Time Series
    # ========================================================
    
    tabPanel(
      
      "County Time Series",
      
      br(),
      
      fluidRow(
        
        column(
          
          3,
          
          div(
            
            class = "control-panel",
            
            selectizeInput(
              
              "timeseries_counties",
              
              "Counties:",
              
              choices =
                APP_DATA$available_counties,
              
              multiple = TRUE,
              
              options =
                list(
                  placeholder =
                    "All counties"
                )
            )
          )
        ),
        
        
        column(
          
          9,
          
          plotlyOutput(
            "county_timeseries",
            height = 580
          )
        )
      )
    ),
    
    
    # ========================================================
    # Tables
    # ========================================================
    
    tabPanel(
      
      "Tables",
      
      br(),
      
      h3(
        "County-Level Data"
      ),
      
      DTOutput(
        "county_table"
      ),
      
      br(),
      
      h3(
        "Bing Tile Data"
      ),
      
      DTOutput(
        "bing_table"
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # County Time Choices
  # ==========================================================
  
  observeEvent(
    
    input$county_date,
    
    {
      
      selected_date <-
        as.Date(
          input$county_date
        )
      
      
      available <-
        APP_DATA$county_df |>
        
        filter(
          
          as.Date(
            datetime,
            tz = "America/New_York"
          ) ==
            selected_date
        ) |>
        
        distinct(
          datetime
        ) |>
        
        arrange(
          datetime
        )
      
      
      req(
        nrow(available) > 0
      )
      
      
      choices <-
        setNames(
          
          as.character(
            available$datetime
          ),
          
          format_time_choice(
            available$datetime
          )
        )
      
      
      updateSelectInput(
        
        session,
        
        "county_hour",
        
        choices = choices,
        
        selected =
          unname(
            choices[1]
          )
      )
    },
    
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # Bing Time Choices
  # ==========================================================
  
  observeEvent(
    
    input$scatter_date,
    
    {
      
      selected_date <-
        as.Date(
          input$scatter_date
        )
      
      
      available <-
        APP_DATA$scatter_df |>
        
        filter(
          
          as.Date(
            datetime,
            tz = "America/New_York"
          ) ==
            selected_date
        ) |>
        
        distinct(
          datetime
        ) |>
        
        arrange(
          datetime
        )
      
      
      req(
        nrow(available) > 0
      )
      
      
      choices <-
        setNames(
          
          as.character(
            available$datetime
          ),
          
          format_time_choice(
            available$datetime
          )
        )
      
      
      updateSelectInput(
        
        session,
        
        "scatter_hour",
        
        choices = choices,
        
        selected =
          unname(
            choices[1]
          )
      )
    },
    
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # Selected Datetimes
  # ==========================================================
  
  county_selected_datetime <-
    reactive({
      
      req(
        input$county_hour
      )
      
      as.POSIXct(
        input$county_hour,
        tz = "America/New_York"
      )
    })
  
  
  scatter_selected_datetime <-
    reactive({
      
      req(
        input$scatter_hour
      )
      
      as.POSIXct(
        input$scatter_hour,
        tz = "America/New_York"
      )
    })
  
  
  # ==========================================================
  # County Map Data
  # ==========================================================
  
  county_map_data <-
    reactive({
      
      selected_datetime <-
        county_selected_datetime()
      
      
      values <-
        APP_DATA$county_df |>
        
        filter(
          datetime ==
            selected_datetime
        )
      
      
      APP_DATA$counties |>
        
        inner_join(
          values,
          by = "county_geoid"
        )
      
    }) |>
    
    bindCache(
      input$county_hour
    )
  
  
  # ==========================================================
  # County Base Map
  # ==========================================================
  
  output$county_map <-
    renderLeaflet({
      
      leaflet(
        
        options =
          leafletOptions(
            preferCanvas = TRUE
          )
      ) |>
        
        addTiles() |>
        
        setView(
          lng = -85.5,
          lat = 36.5,
          zoom = 6
        )
    })
  
  
  outputOptions(
    output,
    "county_map",
    suspendWhenHidden = FALSE
  )
  
  
  # ==========================================================
  # County Map Update
  # ==========================================================
  
  observe({
    
    req(
      input$county_hour
    )
    
    
    x <-
      county_map_data()
    
    
    req(
      nrow(x) > 0
    )
    
    
    fill_colors <-
      percent_colors(
        x$percent_change,
        COUNTY_PERCENT_SCALE
      )
    
    
    # --------------------------------------------------------
    # County Popup
    # --------------------------------------------------------
    
    popup <-
      paste0(
        
        "<b>",
        x$county_name_acs,
        ", ",
        x$county_state,
        "</b>",
        
        "<br><br>",
        
        "<b>Facebook Population</b>",
        
        "<br>",
        
        "Percent Change: ",
        format_percent_value(
          x$percent_change
        ),
        
        "<br>",
        
        "Users During Crisis: ",
        format_number(
          x$n_crisis
        ),
        
        "<br>",
        
        "Baseline Users: ",
        format_number(
          x$n_baseline
        )
      )
    
    
    proxy <-
      leafletProxy(
        "county_map",
        data = x
      ) |>
      
      clearShapes() |>
      
      clearControls() |>
      
      addPolygons(
        
        fillColor =
          fill_colors,
        
        fillOpacity =
          0.80,
        
        # Gray county boundaries
        color =
          "#777777",
        
        opacity =
          0.85,
        
        weight =
          0.6,
        
        popup =
          popup,
        
        highlightOptions =
          highlightOptions(
            
            weight = 2,
            
            color =
              "#333333",
            
            fillOpacity =
              0.90,
            
            bringToFront =
              TRUE
          ),
        
        options =
          pathOptions(
            interactive = TRUE
          )
      )
    
    
    # --------------------------------------------------------
    # County legend:
    #
    # Uses natural county range, roughly 300-400% at the top.
    # --------------------------------------------------------
    
    add_percent_legend(
      
      proxy,
      
      COUNTY_PERCENT_SCALE,
      
      negative_step = 20,
      
      positive_step = 20,
      
      legend_height = 380,
      
      top_plus = TRUE
    )
  })
  
  
  # ==========================================================
  # Bing Tile Data
  # ==========================================================
  
  scatter_data <-
    reactive({
      
      selected_datetime <-
        scatter_selected_datetime()
      
      
      x <-
        APP_DATA$scatter_df |>
        
        filter(
          datetime ==
            selected_datetime
        )
      
      
      if (
        !is.null(
          input$scatter_counties
        ) &&
        length(
          input$scatter_counties
        ) > 0
      ) {
        
        x <-
          x |>
          
          filter(
            county_name_acs %in%
              input$scatter_counties
          )
      }
      
      
      x
      
    }) |>
    
    bindCache(
      input$scatter_hour,
      input$scatter_counties
    )
  
  
  # ==========================================================
  # Join Bing Geometry
  # ==========================================================
  
  bing_tile_sf <-
    reactive({
      
      x <-
        scatter_data()
      
      
      req(
        nrow(x) > 0
      )
      
      
      x <-
        x |>
        
        filter(
          !is.na(quadkey),
          nzchar(quadkey)
        ) |>
        
        distinct(
          quadkey,
          .keep_all = TRUE
        )
      
      
      APP_DATA$bing_tiles |>
        
        inner_join(
          x,
          by = "quadkey"
        )
      
    }) |>
    
    bindCache(
      input$scatter_hour,
      input$scatter_counties
    )
  
  
  # ==========================================================
  # Bing Base Map
  # ==========================================================
  
  output$bing_map <-
    renderLeaflet({
      
      leaflet(
        
        options =
          leafletOptions(
            preferCanvas = TRUE
          )
      ) |>
        
        addTiles() |>
        
        setView(
          lng = -85.5,
          lat = 36.5,
          zoom = 6
        )
    })
  
  
  outputOptions(
    output,
    "bing_map",
    suspendWhenHidden = FALSE
  )
  
  
  # ==========================================================
  # Bing Tile Map Update
  # ==========================================================
  
  observe({
    
    req(
      input$scatter_hour
    )
    
    
    x <-
      bing_tile_sf()
    
    
    req(
      nrow(x) > 0
    )
    
    
    fill_colors <-
      percent_colors(
        x$percent_change,
        BING_PERCENT_SCALE
      )
    
    
    # --------------------------------------------------------
    # Bing Popup
    # --------------------------------------------------------
    
    popup <-
      paste0(
        
        "<b>",
        x$county_name_acs,
        ", ",
        x$county_state,
        "</b>",
        
        "<br><br>",
        
        "<b>Bing Tile</b>",
        
        "<br>",
        
        "Quadkey: ",
        x$quadkey,
        
        "<br>",
        
        "Percent Change: ",
        format_percent_value(
          x$percent_change
        ),
        
        "<br>",
        
        "Users During Crisis: ",
        format_number(
          x$n_crisis
        ),
        
        "<br>",
        
        "Baseline Users: ",
        format_number(
          x$n_baseline
        )
      )
    
    
    proxy <-
      leafletProxy(
        "bing_map",
        data = x
      ) |>
      
      clearShapes() |>
      
      clearControls() |>
      
      addPolygons(
        
        fillColor =
          fill_colors,
        
        fillOpacity =
          0.80,
        
        # Gray Bing tile boundaries
        color =
          "#777777",
        
        opacity =
          0.75,
        
        weight =
          0.25,
        
        popup =
          popup,
        
        highlightOptions =
          highlightOptions(
            
            weight =
              1.5,
            
            color =
              "#222222",
            
            fillOpacity =
              0.95,
            
            bringToFront =
              TRUE
          ),
        
        options =
          pathOptions(
            interactive = TRUE
          )
      )
    
    
    # --------------------------------------------------------
    # Bing legend:
    #
    # Same 1000%+ scale, but much taller so the 100-point
    # labels have substantially more vertical spacing.
    # --------------------------------------------------------
    
    proxy <-
      add_percent_legend(
        
        proxy,
        
        BING_PERCENT_SCALE,
        
        negative_step =
          20,
        
        positive_step =
          50,
        
        legend_height =
          500,
        
        top_plus =
          TRUE
      )
    
    
    # --------------------------------------------------------
    # Zoom to selected county/counties
    # --------------------------------------------------------
    
    if (
      !is.null(
        input$scatter_counties
      ) &&
      length(
        input$scatter_counties
      ) > 0
    ) {
      
      bounds <-
        sf::st_bbox(
          x
        )
      
      
      proxy |>
        
        fitBounds(
          
          lng1 =
            bounds[["xmin"]],
          
          lat1 =
            bounds[["ymin"]],
          
          lng2 =
            bounds[["xmax"]],
          
          lat2 =
            bounds[["ymax"]]
        )
    }
  })
  
  
  # ==========================================================
  # County Time Series
  #
  # Start date is defined in data.R as February 1, 2026.
  # ==========================================================
  
  time_series_data <-
    reactive({
      
      x <-
        APP_DATA$county_df |>
        
        filter(
          datetime >=
            APP_DATA$time_series_start
        )
      
      
      if (
        !is.null(
          input$timeseries_counties
        ) &&
        length(
          input$timeseries_counties
        ) > 0
      ) {
        
        x <-
          x |>
          
          filter(
            county_name_acs %in%
              input$timeseries_counties
          )
      }
      
      
      x
      
    }) |>
    
    bindCache(
      input$timeseries_counties
    )
  
  
  output$county_timeseries <-
    renderPlotly({
      
      x <-
        time_series_data()
      
      
      req(
        nrow(x) > 0
      )
      
      
      plot_ly(
        
        data =
          x,
        
        x =
          ~datetime,
        
        y =
          ~percent_change,
        
        split =
          ~county_name_acs,
        
        type =
          "scattergl",
        
        mode =
          "lines",
        
        hovertemplate =
          paste0(
            
            "<b>%{fullData.name}</b><br>",
            
            "Time: %{x}<br>",
            
            "Percent Change: %{y:.2f}%",
            
            "<extra></extra>"
          )
        
      ) |>
        
        layout(
          
          xaxis =
            list(
              
              title =
                "Date and Time",
              
              range =
                c(
                  
                  APP_DATA$time_series_start,
                  
                  max(
                    x$datetime,
                    na.rm = TRUE
                  )
                )
            ),
          
          yaxis =
            list(
              title =
                "Percent Change"
            )
        )
    })
  
  
  # ==========================================================
  # County Table
  # ==========================================================
  
  output$county_table <-
    renderDT({
      
      datatable(
        
        APP_DATA$county_df,
        
        options =
          list(
            
            pageLength =
              15,
            
            scrollX =
              TRUE,
            
            deferRender =
              TRUE
          ),
        
        rownames =
          FALSE
      )
    })
  
  
  # ==========================================================
  # Bing Tile Table
  # ==========================================================
  
  output$bing_table <-
    renderDT({
      
      datatable(
        
        APP_DATA$scatter_df,
        
        options =
          list(
            
            pageLength =
              15,
            
            scrollX =
              TRUE,
            
            deferRender =
              TRUE
          ),
        
        rownames =
          FALSE
      )
    })
}


# ============================================================
# Run App
# ============================================================

shinyApp(
  ui = ui,
  server = server
)