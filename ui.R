# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
  /* ── Existing: Scope Selector ─────────────────────────────────── */
  #scope-selector {
    margin: 8px 12px; padding: 8px; border-radius: 6px;
    background: #f8f9fa; display: inline-block;
  }

  /* ── Existing: Risk Map Interactive Squares ───────────────────── */
  #risk-sidebar .composite-score {
    text-align: center; font-size: 22px; font-weight: bold;
    margin-bottom: 20px; padding: 12px;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }

  #risk-sidebar .risk-squares {
    display: flex; flex-direction: column; gap: 12px;
  }

  #risk-sidebar .risk-square-container {
    display: flex; flex-direction: column; align-items: stretch;
  }

  #risk-sidebar .risk-square {
    width: 100%; height: 28px; border-radius: 4px;
    display: flex; align-items: center; justify-content: center;
    font-size: 14px; font-weight: bold; color: white;
    text-shadow: 1px 1px 2px rgba(0,0,0,0.5);
    box-shadow: 0 1px 3px rgba(0,0,0,0.2);
  }

  #risk-sidebar .risk1 { background: linear-gradient(135deg, #ff6b6b, #ee5a52); }
  #risk-sidebar .risk2 { background: linear-gradient(135deg, #feca57, #ff9ff3); }
  #risk-sidebar .risk3 { background: linear-gradient(135deg, #48dbfb, #0abde3); }
  #risk-sidebar .risk4 { background: linear-gradient(135deg, #54a0ff, #2e86de); }
  #risk-sidebar .risk5 { background: linear-gradient(135deg, #5f27cd, #341f97); }

  #risk-sidebar .risk-label {
    font-size: 11px; color: #333; font-weight: 500;
    text-align: center; margin-top: 4px;
  }

  #risk-coords {
    margin-top: 15px; padding: 12px; background: white;
    border-radius: 6px; border-left: 4px solid #667eea; font-size: 14px;
  }

  @media (max-width: 768px) {
    #risk-sidebar .risk-square { width: 60px; height: 60px; font-size: 16px; }
  }

  /* ── New: Layer Block Containers ──────────────────────────────── */
  .layer-block {
    border: 1px solid #e0e0e0; border-radius: 6px;
    padding: 8px 10px; margin-bottom: 8px; background: #fafafa;
  }

  .layer-header {
    display: flex; align-items: center; justify-content: space-between;
  }

  .layer-header .form-group { margin-bottom: 0; }

  .toggle-chevron {
    color: #666; font-size: 13px; padding: 0; line-height: 1;
  }

  /* ── New: Layer Parameter Panel ───────────────────────────────── */
  .layer-params {
    margin-top: 10px; padding-top: 8px;
    border-top: 1px dashed #ddd;
  }

  .layer-params .form-group { margin-bottom: 8px; }

  .layer-params label { font-size: 12px; color: #555; }

  /* ── New: Create Layer Button ─────────────────────────────────── */
  .btn-block { width: 100%; margin-top: 6px; }

  .btn-create {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white; border: none; border-radius: 4px;
    padding: 6px 12px; font-size: 13px; font-weight: 500;
    width: 100%; margin-top: 6px; cursor: pointer;
    box-shadow: 0 1px 3px rgba(0,0,0,0.2);
    transition: opacity 0.15s ease;
  }

  .btn-create:hover { opacity: 0.88; color: white; }

  /* ── New: Section Headers ─────────────────────────────────────── */
  .layer-section-header {
    font-size: 13px; font-weight: 600; color: #444;
    text-transform: uppercase; letter-spacing: 0.04em;
    margin: 12px 0 6px 0; padding-bottom: 4px;
    border-bottom: 2px solid #667eea;
  }
  
  /* ── Map Panel: Sticky map, scrollable sidebar ────────────────── */
  html, body {
  height: 100%;
  overflow: hidden;
}

.container-fluid {
  height: 100vh;
  overflow: hidden;
}

.tab-content {
  height: calc(100vh - 84px); /* 42px = height of navbar tabs */
  overflow: hidden;
}
  
  .well {
    height: 100vh;
    overflow-y: auto;
    position: sticky;
    top: 0;
  }
  
  .col-sm-8 {
    position: sticky;
    top: 0;
    height: 100vh;
  }
  
  #risk_map {
    height: 100vh !important;
  }
"))),
  
  tabsetPanel(
    id = "main_tab",
    tabPanel(
      "Introduction",
      fluidPage(
        titlePanel("Sediment & Water Quality in the Pilcomayo River Basin"),
        tags$hr(),
        tags$img(src = "pilcomayo.jpg", height = "350px"),
        tags$hr(),
        includeMarkdown("text/introduction.md"),
        # load from a .md to reduce clutter
        
        tabPanel("Import", dataUploadUI("upload_data")),
        
        tags$hr(),
        tags$hr(),
        includeMarkdown("text/introduction_sources.md"),
        tags$hr(),
        tags$p(
          "This application was developed using R Shiny and integrates spatial and tabular data for interactive analysis."
        )
      )
    ),
    
    # In ui.R - replace the Map of Environmental Samples tab:
    tabPanel(
      "Map of Environmental Samples",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "!output.map_data_ready",
            div(
              style = "text-align: center; padding: 20px;",
              icon("spinner", class = "fa-spin fa-3x"),
              h4("Loading data...", style = "margin-top: 20px;")
            )
          ),
          
          conditionalPanel(
            condition = "output.map_data_ready",
            
            # MEDIA SELECTOR - at the top
            radioButtons(
              "plot_media",
              "Media:",
              choices = c("Water" = "water", "Sediment" = "sediment"),
              selected = "sediment",
              inline = TRUE
            ),
            
            # DATA SCOPE SELECTOR
            radioButtons(
              "plot_data_scope",
              "Data Scope:",
              choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
              selected = "bol",
              inline = TRUE
            ),
            
            # SEDIMENT SIDEBAR - only show when sediment is selected
            conditionalPanel(
              condition = "input.plot_media == 'sediment'",
              
              uiOutput("sed_campaign_ui"),
              uiOutput("tamiz_ui"),
              selectInput("sed_metal", "Select Parameter:", choices = NULL),
              radioButtons(
                "sed_value_type",
                "Symbolize by:",
                choices = c(
                  "Measured Concentration (mg/kg)" = "sed_value",
                  "Compare to USGS SQGs" = "usgs",
                  "Hazard Quotient (HQ)" = "hq"
                ),
                selected = "sed_value"
              ),
              uiOutput("sed_legend"),
              info_callout(
                "Sediment Quality Map",
                "This map displays sediment quality parameters from monitoring campaigns.
            Circle size represents the measured concentration, while colors can show either
            raw values or comparison to USGS Sediment Quality Guidelines
            (TEL/PEL thresholds). Data can be filtered by date range and sieve size.
            Data is sourced from www2.pilcomayo.net."
              )
            ),
            
            # WATER SIDEBAR - only show when water is selected
            conditionalPanel(
              condition = "input.plot_media == 'water'",
              
              uiOutput("water_campaign_ui"),
              selectInput("water_metal", "Select Parameter:", choices = NULL),
              radioButtons(
                "water_value_type",
                "Symbolize by:",
                choices = c(
                  "Measured Concentration" = "water_value",
                  "Compare to Bolivian Standards" = "water_class",
                  "Hazard Quotient (HQ)" = "hq"
                ),
                selected = "water_value"
              ),
              uiOutput("water_legend"),
              info_callout(
                "Water Quality Map",
                "This map displays water quality parameters from monitoring campaigns.
            Circle size represents the measured concentration, while colors can show either
            raw values or classification based on Bolivian standards (Ley 1333).
            Data can be filtered by date range.
            Data is sourced from www2.pilcomayo.net."
              )
            )
          )
        ),
        
        mainPanel(
          # SEDIMENT content
          conditionalPanel(
            condition = "input.plot_media == 'sediment'",
            tabsetPanel(
              tabPanel("Map", leafletOutput("sed_map", height = 600)),
              tabPanel("Table", dataTableOutput("sed_table")),
              tabPanel("Sediment Quality Standards", dataTableOutput("stds_sed_table"))
            )
          ),
          
          # WATER content
          conditionalPanel(
            condition = "input.plot_media == 'water'",
            tabsetPanel(
              tabPanel("Map", leafletOutput("water_map", height = 600)),
              tabPanel("Table", dataTableOutput("water_table")),
              tabPanel("Water Quality Standards", dataTableOutput("stds_1333_table"))
            )
          )
        )
      )
    ),
    # Time Series tab
    tabPanel(
      "Time Series", 
      sidebarLayout(
        sidebarPanel(
          # Add Data Scope at the top
          radioButtons(
            "plot_data_scope",
            "Data Scope:",
            choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
            selected = "bol",
            inline = TRUE
          ),
          selectInput("ts_station", "Select Station:", choices = NULL),
          selectInput("ts_param", "Select Parameter:", choices = NULL),
          conditionalPanel(
            condition = "input.ts_tabs == 'Sediment Samples'",
            checkboxInput("ts_tamiz_checkbox", "Filter by Sieve Size", value = FALSE)
          ),
          conditionalPanel(
            condition = "input.ts_tamiz_checkbox == true && input.ts_tabs == 'Sediment Samples'",
            selectInput("ts_tamiz", "Select Sieve Size:", choices = NULL)
          ),
          selectInput("ts_standard_mode", "Apply Standards:",
                      choices = c(
                        "All" = "all",
                        "Strict" = "strict",
                        "None" = "none",
                        "Bolivian 1333" = "bol",
                        "EPA" = "epa",
                        "WHO" = "who",
                        "USGS" = "usgs",
                        "FAO" = "fao"
                      ),
                      selected = "none"
          ),
          
          # Descriptive text for each time series tab
          conditionalPanel(
            condition = "input.ts_tabs == 'Water Samples'",
            info_callout(
              "Water Quality Time Series",
              "This plot displays the temporal variation of water quality parameters
                                    at the selected monitoring station. Points represent individual measurements,
                                    while the line shows the trend over time. Reference standards from Bolivian Ley No. 1333
                                    are shown as dashed horizontal lines (where applicable). Data is sourced from www2.pilcomayo.net."
            )
          ),
          
          conditionalPanel(
            condition = "input.ts_tabs == 'Sediment Samples'",
            info_callout(
              "Sediment Quality Time Series",
              "This plot displays the temporal variation of sediment quality parameters
                                    at the selected monitoring station. Individual points show measurements,
                                    with the connecting line representing daily averages when multiple samples
                                    exist per date. Darker points represent samples taken further from the river bank (when variable).
                                    USGS Sediment Quality Guidelines (TEL/PEL) are shown as dashed horizontal lines (where applicable).
                                    Data is sourced from www2.pilcomayo.net."
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "ts_tabs",
            tabPanel("Time Series", 
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("ts_plot_water"),
                         uiOutput("ts_plot_sed")
                       )
                     )
            ),
            tabPanel("View Standards", dataTableOutput("stds_all"))
          )
        )
      )
    ),  # Close Time Series tabPanel
    
    # Ranking Plots tab
    tabPanel(
      "Ranking Plots",
      
      # Add Data Scope at the top
      fluidRow(
        column(
          12,
          radioButtons(
            "plot_data_scope",
            "Data Scope:",
            choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
            selected = "bol",
            inline = TRUE
          )
        )
      ),
      
      tabsetPanel(
        tabPanel("Worst Observations", fluidRow(
          column(
            4,
            # radioButtons(
            #   "observation_std",
            #   "Rank Observations Using:",
            #   choices = c(
            #     "Raw Water Samples" = "value",
            #     "Bolivian Water Standards" = "class",
            #     "Raw Sediment Samples" = "sed_value",
            #     "USGS SQGs" = "usgs",
            #     "Hazard Quotients" = "hq" # NADAV: NEED TO FIX
            #   )
            # ),
            # conditionalPanel(
            #   condition = "input.observation_std == 'class'",
            #   radioButtons(
            #     "observation_plot_class",
            #     "Rank by:",
            #     choices = c(
            #       "Worst Overall Score" = "worst_score",
            #       "# Unclassified Parameters" = "unclassified",
            #       "# Class D Parameters" = "class_d",
            #       "# Class C Parameters" = "class_c",
            #       "# Class B Parameters" = "class_b"
            #     )
            #   )
            # ),
            # conditionalPanel(
            #   condition = "input.observation_std == 'value'",
            #   selectInput("observation_plot_param", "Select Parameter:", choices = NULL)
            # ),
            # conditionalPanel(
            #   condition = "input.observation_std == 'usgs'",
            #   radioButtons(
            #     "observation_plot_usgs",
            #     "Rank by:",
            #     choices = c(
            #       "Worst Overall Score" = "worst_score",
            #       "# Parameters Above PEL" = "above_pel",
            #       "# Parameters Above TEL" = "above_tel"
            #     )
            #   )
            # ),
            selectInput("observation_plot_param", "Select Parameter:", choices = NULL),
            # conditionalPanel(
            #   condition = "input.observation_std == 'sed_value'",
            #   selectInput("observation_plot_param_sed", "Select Parameter:", choices = NULL)
            # ),
            # conditionalPanel( # NADAV: NEED TO FIX
            #   condition = "input.observation_std == 'hq'",
            #   radioButtons(
            #     "observation_plot_hq",
            #     "Rank by:",
            #     choices = c(
            #       "Greatest total Hazard Quotient" = "highest_hq",
            #       "# Parameters above standard" = "quantity_hq"
            #     )
            #   )
            # ),
            # conditionalPanel(
            #   condition = "input.observation_std == 'class'",
            #   info_callout(
            #     "Observation Ranking",
            #     "This plot ranks individual water samples based on water quality standards from Bolivian law.
            #                                         Observations can be ranked by the number of parameters that fall into each classification, or by overall score.
            #                                           Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each observation.
            #                                           Data is sourced from www2.pilcomayo.net."
            #   )
            # ),
            info_callout(
              "Observation Ranking",
              "This plot ranks Hazard Quotients for individual samples (based on sampled data for the selected parameter against the strictest standard).
                                                    Data is sourced from www2.pilcomayo.net."
            )
            #   conditionalPanel(
            #     condition = "input.observation_std == 'usgs'",
            #     info_callout(
            #       "Observation Ranking",
            #       "This plot ranks individual sediment samples based on USGS Sediment Quality Guidelines (SQGs).
            #                                           Observations can be ranked by the number of parameters that fall into each category, or by overall score.
            #                                             Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above Pel=2), and finding the mean value for each observation.
            #                                             Data is sourced from www2.pilcomayo.net."
            #     )
            #   ),
            #   conditionalPanel(
            #     condition = "input.observation_std == 'sed_value'",
            #     info_callout(
            #       "Observation Ranking",
            #       "This plot ranks individual sediment samples based on measured values of the selected sediment quality parameter.
            #                                        Higher values are ranked worse.
            #                                        Data is sourced from www2.pilcomayo.net."
            #     )
            #   )
          ),
          column(8, plotlyOutput("observation_scores_plot", height = "500px"))
        )),  # Close Worst Observations tabPanel
        
        tabPanel("Worst Stations", fluidRow(
          column(
            4,
            selectInput("station_plot_param", "Select Parameter:", choices = NULL),
            selectInput("station_plot_media", "Select Media:", choices = c("All Media" = "all", "Water" = "water", "Sediment" = "sed")),
            conditionalPanel(
              condition = "input.station_plot_media != 'sed'",
              selectInput("station_plot_fraction", "Select Fraction:", choices = c("All Fractions" = "any", "Total" = "Total", "Dissolved" = "Dissolved", "Suspended" = "Suspended"))
            ),
            radioButtons( # 1/8/2026: changed from param_plot_method
              "station_plot_method_temporal",
              "Temporal Aggregation:",
              choices = c("Most Recent Data" = "recent", "Average Value" = "average", "Most Extreme Value" = "max") # recent, mean, average, max, weighted
            ),
            radioButtons( # 1/8/2026: changed from param_plot_method
              "station_plot_method_parameter",
              "Parameter Aggregation:",
              choices = c("Average Value" = "mean", "Most Extreme Value" = "max", "95th Percentile" = "pct95") # mean, average, median, max
            ),
            # radioButtons( # 1/8/2026: changed from param_plot_method
            #   "station_plot_method",
            #   "OLD METHOD",
            #   choices = c("Average Value" = "average", "Most Extreme Value" = "max")
            # ),
            info_callout(
              "Station Ranking",
              "This plot ranks water sampling stations based on water quality standards from Bolivian law.
                                                    Stations can be ranked by the mean number of parameters that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each station.
                                                      For Overall Score, weighted mean can be used instead to emphasize recent observations (weight = 1 / (1 + years since present)).
                                                      Data is sourced from www2.pilcomayo.net."
            ), # comma is for the brackets, remove if it's the last thing here
            
            { # old code
              # radioButtons(
              #   "station_plot_type",
              #   "Rank Stations Using:",
              #   choices = c(
              #     "Raw Water Samples" = "value",
              #     "Bolivian Water Standards" = "class",
              #     "Raw Sediment Samples" = "sed_value",
              #     "USGS SQGs" = "usgs"
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'class'",
              #   radioButtons(
              #     "station_plot_class",
              #     "Rank by:",
              #     choices = c(
              #       "Worst Overall Score" = "worst_score",
              #       "Mean # Unclassified Parameters" = "unclassified",
              #       "Mean # Class D Parameters" = "class_d",
              #       "Mean # Class C Parameters" = "class_c",
              #       "Mean # Class B Parameters" = "class_b"
              #     )
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'class' && input.station_plot_class == 'worst_score'",
              #   checkboxInput(
              #     "station_plot_recency",
              #     "Weigh recent observations higher",
              #     value = FALSE
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'value'",
              #   selectInput("station_plot_param", "Select Parameter:", choices = NULL),
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'usgs'",
              #   radioButtons(
              #     "station_plot_usgs",
              #     "Rank by:",
              #     choices = c(
              #       "Worst Overall Score" = "worst_score",
              #       "Mean # Parameters Above PEL" = "above_pel",
              #       "Mean # Parameters Above TEL" = "above_tel"
              #     )
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'usgs' && input.station_plot_usgs == 'worst_score'",
              #   checkboxInput(
              #     "station_plot_recency_sed",
              #     "Weigh recent observations higher",
              #     value = FALSE
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'sed_value'",
              #   selectInput("station_plot_param_sed", "Select Parameter:", choices = NULL)
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'value' | input.station_plot_type == 'sed_value'",
              #   radioButtons(
              #     "station_param_type",
              #     "Rank by:",
              #     choices = c("Average Value" = "average", "Most Extreme Value" = "max")
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'class'",
              #   info_callout(
              #     "Station Ranking",
              #     "This plot ranks water sampling stations based on water quality standards from Bolivian law.
              #                                         Stations can be ranked by the mean number of parameters that fall into each classification, or by overall score.
              #                                           Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each station.
              #                                           For Overall Score, weighted mean can be used instead to emphasize recent observations (weight = 1 / (1 + years since present)).
              #                                           Data is sourced from www2.pilcomayo.net."
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'value'",
              #   info_callout(
              #     "Station Ranking",
              #     "This plot ranks water sampling stations based on measured values of the selected parameter.
              #                                         For most parameters, higher values are ranked higher (worse). However, for some (DO, pH, etc.), lower values are ranked higher (worse).
              #                                           Stations can be ranked based on the most extreme/worst recorded value, or the mean value across observations at that station.
              #                                           Data is sourced from www2.pilcomayo.net."
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'usgs'",
              #   info_callout(
              #     "Station Ranking",
              #     "This plot ranks sediment sampling stations based on USGS Sediment Quality Guidelines (SQGs).
              #                                         Stations can be ranked by the mean number of parameters that fall into each category, or by overall score.
              #                                           Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above PEL=2), and finding the mean value for each station.
              #                                           For Overall Score, weighted mean can be used instead to emphasize recent observations (weight = 1 / (1 + years since present)).
              #                                           Data is sourced from www2.pilcomayo.net."
              #   )
              # ),
              # conditionalPanel(
              #   condition = "input.station_plot_type == 'sed_value'",
              #   info_callout(
              #     "Station Ranking",
              #     "This plot ranks sediment sampling stations based on measured values of the selected parameter.
              #                                         Higher values are ranked worse.
              #                                           Stations can be ranked based on the most highest recorded value, or the mean value across observations at that station.
              #                                           Data is sourced from www2.pilcomayo.net."
              #   )
              # )
            } # old code.
          ),
          column(8, plotlyOutput("station_scores_plot", height = "500px"))
        )),  # Close Worst Stations tabPanel
        
        tabPanel("Worst Parameters", fluidRow(
          column(
            4,
            selectInput("param_plot_station", "Select Station:", choices = c("All Stations" = "all")),
            selectInput("param_plot_media", "Select Media:", choices = c("All Media" = "all", "Water" = "water", "Sediment" = "sed")),
            conditionalPanel(
              condition = "input.param_plot_media != 'sed'",
              selectInput("param_plot_fraction", "Select Fraction:", choices = c("All Fractions" = "all", "Total" = "Total", "Dissolved" = "Dissolved", "Suspended" = "Suspended"))
            ),
            radioButtons( 
              "param_plot_method_temporal",
              "Rank Data by Year Using:",
              choices = c("Most Recent Year Only" = "recent", "Average Value Across Time" = "average", "Most Extreme Year" = "max", "Time-Weighted Value" = "weighted")
            ),
            radioButtons(
              "param_plot_method_spatial",
              "Rank by:", # Nadav's Note: Better way to state?
              choices = c("Average Value" = "average", "Most Extreme Value" = "max", "Median Value" = "median")
            ),
            shinyBS::bsCollapse(id = "param_plot_adv",
                                bsCollapsePanel(title = "⚙️ Advanced Settings",
                                                sliderInput("decay_per_day", "Decay/day", 0, 0.1, 0.001),
                                                style = "default"  # Grey! Down/up arrows [web:18][web:24]
                                                )
            ),
            info_callout(
              "Parameter Ranking",
              "TO UPDATE: This plot ranks water quality parameters based on standards from Bolivian law.
                                                    Parameters can be ranked by the percent of observations that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each parameter.
                                                      Light bars represent percents/scores calculated after omitting NA rows for that parameter.
                                                      Data is sourced from www2.pilcomayo.net."
            )),
          column(8, plotlyOutput("param_scores_plot", height = "500px"))
        )),  # Close Worst Parameters tabPanel
        
        tabPanel("Worst Sieve Sizes", fluidRow(
          column(
            4,
            selectInput("sieve_plot_param", "Select Parameter:", choices = c("All Parameters" = "all")),
            selectInput("sieve_plot_station", "Select Station:", choices = c("All Stations" = "all")),
            radioButtons(
              "sieve_plot_method",
              "Rank by:",
              choices = c("Average Value" = "avg", "Most Extreme Value" = "max")
            )
          ),
          column(8, plotlyOutput("sieve_scores_plot", height = "500px"))
        ))  # Close Worst Sieve Sizes tabPanel
      )  # Close tabsetPanel for Ranking Plots
    ),  # Close Ranking Plots tabPanel
    
    # PCA tab
    tabPanel(
      "Principal Component Analysis",
      sidebarLayout(
        sidebarPanel(
          # Add Data Scope at the top
          radioButtons(
            "plot_data_scope",
            "Data Scope:",
            choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
            selected = "bol",
            inline = TRUE
          ),
          selectInput("pca_media", "Select Media:", choices = c("All Media" = "all", "Water" = "water", "Sediment" = "sediment")),
          selectInput("pca_station", "Select Station:", choices = c("All Stations" = "all")),
          selectizeInput(
            "pca_parameters",
            "Select Parameters for PCA:",
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 15)
          ),
          actionButton("deselect_all_pca", "Clear Selection"),
          br(),
          br(),
          actionButton("run_pca", "Run PCA", class = "btn-primary"),
          
          info_callout(
            "Principal Component Analysis",
            "This analysis performs PCA on selected water quality parameters to identify
                                    underlying patterns and relationships in the data. Missing values are
                                    imputed using optimal component estimation. The variable plot shows parameter
                                    contributions and correlations, colored by representation quality (cos²).
                                    The scree plot displays variance explained by each component to help determine
                                    the optimal number of dimensions. Select up to 15 parameters and click 'Run PCA'
                                    to begin the analysis. Data is sourced from www2.pilcomayo.net."
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Autoplot", 
                     plotlyOutput("pca_plot"),
                     plotOutput("pca_static", height = "600px")  # Static below interactive
            ),
            tabPanel("Scree Plot", plotOutput("scree_plot"))
          )
        )
      ) # close sidebarLayout 
    ), # Close PCA tabPanel
    
    
    
    
    
    ################## RISK MAPPING ############################################
    
    ################# RISK MAPPING ##############################################
    
    ################ RISK MAPPING #################################################
    
    
    
    
    
    tabPanel(
      "Risk Scores Map",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "!output.map_data_ready",
            div(
              style = "text-align: center; padding: 20px;",
              icon("spinner", class = "fa-spin fa-3x"),
              h4("Loading data...", style = "margin-top: 20px;")
            )
          ),
          
          conditionalPanel(
            condition = "output.map_data_ready",
            
            h3(strong("Map Layers")),
            
            # ── Water Risk ────────────────────────────────────────────────
            h5(strong("Water Pollution Risk"), style = "margin-bottom: 6px; color: #1C3EB8;"),

            # Water Sampling Stations
            div(class = "layer-block",
                checkboxInput("risk_water_stations", "Sampling Stations (Water)", value = FALSE),
                conditionalPanel(
                  condition = "input.risk_water_stations == true",
                  actionButton("create_water_stations", "Create Layer",
                               class = "btn-create",
                               icon = icon("layer-group")),
                  checkboxInput("show_water_stations_inputs", "Modify Inputs", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_water_stations_inputs == true",
                    div(class = "layer-params",
                        uiOutput("water_stations_params_ui"),
                        helpText("Select 'All Parameters' to include every measured contaminant, or choose specific ones to target your analysis."),
                        selectInput("water_stations_temp_ag", "Temporal Aggregation:",
                                    choices = c(
                                      "Recent" = "recent",
                                      "Average" = "mean"
                                    )),
                        helpText("How to handle parameters that were repeatedly sampled at the same location across time. Select 'Recent' to ignore older data. Select 'Average' to take the average across time for each station-parameter combination."),
                        conditionalPanel(
                          condition = "input.water_stations_temp_ag == 'recent'",
                          numericInput("water_stations_nyears", "Years of Data to Include:",
                                       value = 5, min = 1, max = 20, step = 1),
                          helpText(HTML("Leave blank to use only the single most recent sample per station.
                Enter a number (e.g. 5) to include all samples from the past N years.<br><br>
                <i>Note: when including multiple years of data, the final aggregation method will pool across both parameters and time — for example, 'Max' will return the single highest HQ value observed across all parameters and all sampling events within the specified time window.</i>"))
                        ),
                        selectInput("water_stations_param_ag", "Final Aggregation:",
                                    choices = c(
                                      "Average" = "mean",
                                      "Max" = "max",
                                      "95th Percentile" = "pct95"
                                    ),
                                    selected = "pct95"),
                        helpText("How to aggregate hazard scores after temporal aggregation. Each station will be scored using either the average value, maximum value, or 95th percentile across values at that location."),
                        selectInput("water_stations_fraction", "Fraction:",
                                    choices = c("All", "Dissolved", "Suspended")),
                        helpText("Select a fraction if you are only interested in dissolved or suspended concentrations. Otherwise, select 'All'.")
                    )
                  )
                )
            ),
            # Water Risk Interpolation
            div(class = "layer-block",
                checkboxInput("risk_water", "Interpolated Risk (Water)", value = FALSE),
                conditionalPanel(
                  condition = "input.risk_water == true",
                  actionButton("create_water", "Create Layer",
                               class = "btn-create",
                               icon = icon("layer-group")),
                  checkboxInput("show_water_inputs", "Modify Inputs", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_water_inputs == true",
                    div(class = "layer-params",
                        uiOutput("water_params_ui"),
                        helpText("Select 'All Parameters' to include every measured contaminant, or choose specific ones to target your analysis."),
                        selectInput("water_temp_ag", "Temporal Aggregation:",
                                    choices = c(
                                      "Recent" = "recent",
                                      "Average" = "mean"
                                    )),
                        helpText("How to handle parameters that were repeatedly sampled at the same location across time. Select 'Recent' to ignore older data. Select 'Average' to take the average across time for each station-parameter combination."),
                        conditionalPanel(
                          condition = "input.water_temp_ag == 'recent'",
                          numericInput("water_nyears", "Years of Data to Include:",
                                       value = 5, min = 1, max = 20, step = 1),
                          helpText(HTML("Leave blank to use only the single most recent sample per station.
              Enter a number (e.g. 5) to include all samples from the past N years.<br><br>
              <i>Note: when including multiple years of data, the final aggregation method will pool across both parameters and time — for example, 'Max' will return the single highest HQ value observed across all parameters and all sampling events within the specified time window.</i>")
                                   )),
                        selectInput("water_param_ag", "Final Aggregation:",
                                    choices = c(
                                      "Average" = "mean",
                                      "Max" = "max",
                                      "95th Percentile" = "pct95"
                                    ),
                                    selected = "pct95"),
                        helpText("How to aggregate hazard scores after temporal aggregation. Each station will be scored using either the average value, maximum value, or 95th percentile across values at that location."),
                        selectInput("water_fraction", "Fraction:",
                                    choices = c("All", "Dissolved", "Suspended")),
                        helpText("Select a fraction if you are only interested in dissolved or suspended concentrations. Otherwise, select 'All'.")
                    )
                  )
                )
            ),
            
            # ── Sediment Risk ────────────────────────────────────────────────
            h5(strong("Sediment Pollution Risk"), style = "margin-bottom: 6px; color: #1C8C27;"),
            # Sediment Sampling Stations
            div(class = "layer-block",
                checkboxInput("risk_sed_stations", "Sampling Stations (Sediment)", value = FALSE),
                conditionalPanel(
                  condition = "input.risk_sed_stations == true",
                  actionButton("create_sed_stations", "Create Layer",
                               class = "btn-create",
                               icon = icon("layer-group")),
                  checkboxInput("show_sed_stations_inputs", "Modify Inputs", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_sed_stations_inputs == true",
                    div(class = "layer-params",
                        uiOutput("sed_stations_params_ui"),
                        helpText("Select 'All Parameters' to include every measured contaminant, or choose specific ones to target your analysis."),
                        selectInput("sed_stations_temp_ag", "Temporal Aggregation:",
                                    choices = c(
                                      "Recent" = "recent",
                                      "Average" = "mean"
                                    )),
                        helpText("How to handle parameters that were repeatedly sampled at the same location across time. Select 'Recent' to ignore older data. Select 'Average' to take the average across time for each station-parameter combination."),
                        conditionalPanel(
                          condition = "input.sed_stations_temp_ag == 'recent'",
                          numericInput("sed_stations_nyears", "Years of Data to Include:",
                                       value = 5, min = 1, max = 20, step = 1),
                          helpText(HTML("Leave blank to use only the single most recent sample per station.
                Enter a number (e.g. 5) to include all samples from the past N years.<br><br>
                <i>Note: when including multiple years of data, the final aggregation method will pool across both parameters and time — for example, 'Max' will return the single highest HQ value observed across all parameters and all sampling events within the specified time window.</i>"))
                        ),
                        selectInput("sed_stations_param_ag", "Final Aggregation:",
                                    choices = c(
                                      "Average" = "mean",
                                      "Max" = "max",
                                      "95th Percentile" = "pct95"
                                    ),
                                    selected = "pct95"),
                        helpText("How to aggregate hazard scores after temporal aggregation. Each station will be scored using either the average value, maximum value, or 95th percentile across values at that location.")
                    )
                  )
                )
            ),
            # Sediment Risk Interpolation
            div(class = "layer-block",
                checkboxInput("risk_sediment", "Interpolated Risk (Sediment)", value = FALSE),
                conditionalPanel(
                  condition = "input.risk_sediment == true",
                  actionButton("create_sediment", "Create Layer",
                               class = "btn-create",
                               icon = icon("layer-group")),
                  checkboxInput("show_sed_inputs", "Modify Inputs", value = FALSE),
                  conditionalPanel(
                    condition = "input.show_sed_inputs == true",
                    div(class = "layer-params",
                        uiOutput("sed_params_ui"),
                        helpText("Select 'All Parameters' to include every measured contaminant, or choose specific ones to target your analysis."),
                        selectInput("sed_temp_ag", "Temporal Aggregation:",
                                    choices = c(
                                      "Recent" = "recent",
                                      "Average" = "mean"
                                    )),
                        helpText("How to handle parameters that were repeatedly sampled at the same location across time. Select 'Recent' to ignore older data. Select 'Average' to take the average across time for each station-parameter combination."),
                        conditionalPanel(
                          condition = "input.sed_temp_ag == 'recent'",
                          numericInput("sed_nyears", "Years of Data to Include:",
                                       value = 5, min = 1, max = 20, step = 1),
                          helpText(HTML("Leave blank to use only the single most recent sample per station.
                Enter a number (e.g. 5) to include all samples from the past N years.<br><br>
                <i>Note: when including multiple years of data, the final aggregation method will pool across both parameters and time — for example, 'Max' will return the single highest HQ value observed across all parameters and all sampling events within the specified time window.</i>"))
                        ),
                        selectInput("sed_param_ag", "Final Aggregation:",
                                    choices = c(
                                      "Average" = "mean",
                                      "Max" = "max",
                                      "95th Percentile" = "pct95"
                                    ),
                                    selected = "pct95"),
                        helpText("How to aggregate hazard scores after temporal aggregation. Each station will be scored using either the average value, maximum value, or 95th percentile across values at that location.")
                    )
                  )
                )
            ),
            
            
            h5(strong("Population Vulnerability"), style = "margin-bottom: 6px; color: #721FAB;"),
            # EJI Vulnerability (polygon, preloaded)
            checkboxInput("risk_eji", "EJI Scored Municipalities", value = FALSE),
            
            # Population Density (raster, preloaded)
            checkboxInput("risk_pop_density", "Square Root of Population Density", value = FALSE),
            
            hr(),
            
            # ── OTHER ─────────────────────────────────────────────────
            h5(strong("Other"), style = "margin-bottom: 6px;"),
            checkboxInput("risk_mines",      "Mine Locations",        value = FALSE),
            checkboxInput("risk_tailings",   "Tailings Sites",        value = FALSE),
            checkboxInput("risk_settlements","Settlements",           value = FALSE),
            checkboxInput("risk_river",      "River Network",         value = FALSE),
            checkboxInput("risk_air",        "Air Hazard",            value = FALSE),
            
            hr(),
            
            # Click-to-inspect callout
            uiOutput("risk_sidebar"),
            info_callout("Risk Map",
                         "Click anywhere on the map to see detailed risk values for each layer.
                      Higher values = higher risk.")
          )
        ),
        mainPanel(
          leafletOutput("risk_map", height = 800)
        )
      )
    )
  )  # Close tabsetPanel
)  # Close fluidPage