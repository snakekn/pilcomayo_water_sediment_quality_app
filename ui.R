# Define UI
ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
    /* small styling for the selector box (tweak as needed) */
    #scope-selector { margin: 8px 12px; padding: 8px; border-radius: 6px; background:#f8f9fa; display:inline-block; }
  "
    )
  )),
  
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
            tabPanel("View Standards", dataTableOutput("stds_all")), # this should stay :)
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
              selectInput("station_plot_fraction", "Select Fraction:", choices = c("All Fractions" = "any", "Total" = "Total", "Dissolved" = "Dissolved", "Suspended" = "Suspended")),
            ),
            radioButtons(
                  "station_plot_method",
                  "Rank by:",
                  choices = c("Average Value" = "average", "Most Extreme Value" = "max")
                ),
              
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
            radioButtons(
              "param_plot_type",
              "Rank Parameters Using:",
              choices = c("Bolivian Water Standards" = "class", "USGS SQGs" = "usgs")
            ),
            conditionalPanel(
              condition = "input.param_plot_type == 'class'",
              radioButtons(
                "param_plot_class",
                "Rank by:",
                choices = c(
                  "Worst Overall Score" = "worst_score",
                  "% Observations Unclassified" = "unclassified",
                  "% Observations in Class D" = "class_d",
                  "% Observations in Class C" = "class_c",
                  "% Observations in Class B" = "class_b"
                )
              )
            ),
            conditionalPanel(
              condition = "input.param_plot_type == 'usgs'",
              radioButtons(
                "param_plot_usgs",
                "Rank by Observations by:",
                choices = c(
                  "Worst Overall Score" = "worst_score",
                  "% Observations Above PEL" = "above_pel",
                  "% Observations Above TEL" = "above_tel"
                )
              )
            ),
            checkboxInput("param_plot_checkbox", "Filter by Station", value = FALSE),
            conditionalPanel(
              condition = "input.param_plot_checkbox == true",
              selectInput("param_plot_station", "Select Station:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.param_plot_type == 'class'",
              info_callout(
                "Parameter Ranking",
                "This plot ranks water quality parameters based on standards from Bolivian law.
                                                    Parameters can be ranked by the percent of observations that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each parameter.
                                                      Light bars represent percents/scores calculated after omitting NA rows for that parameter.
                                                      Data is sourced from www2.pilcomayo.net."
              )
            ),
            conditionalPanel(
              condition = "input.param_plot_type == 'usgs'",
              info_callout(
                "Parameter Ranking",
                "This plot ranks sediment quality parameters based on USGS Sediment Quality Guidelines (SQGs).
                                                    Parameters can be ranked by the percent of observations that fall into each category, or by overall score.
                                                      Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above PEL=2), and finding the mean value for each parameter.
                                                      Light bars represent percents/scores calculated after omitting NA rows for that parameter.
                                                      Data is sourced from www2.pilcomayo.net."
              )
            )
          ),
          column(8, plotlyOutput("param_scores_plot", height = "500px"))
        )),  # Close Worst Parameters tabPanel
        
        tabPanel("Worst Sieve Sizes", fluidRow(
          column(
            4,
            radioButtons(
              "sieve_plot_type",
              "Rank Sieve Sizes Using:",
              choices = c("Raw Sediment Samples" = "sed_value", "USGS SQGs" = "usgs")
            ),
            conditionalPanel(
              condition = "input.sieve_plot_type == 'sed_value'",
              selectInput("sieve_plot_param", "Select Parameter:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.sieve_plot_type == 'sed_value'",
              radioButtons(
                "sieve_param_type",
                "Rank by:",
                choices = c("Average Value" = "avg", "Most Extreme Value" = "max")
              )
            ),
            conditionalPanel(
              condition = "input.sieve_plot_type == 'usgs'",
              radioButtons(
                "sieve_plot_usgs",
                "Rank By:",
                choices = c(
                  "Worst Scored Overall" = "worst_score",
                  "Mean # Observations Above PEL" = "above_pel",
                  "Mean # Observations Above TEL" = "above_tel"
                )
              )
            ),
            conditionalPanel(
              condition = "input.sieve_plot_type == 'usgs'",
              checkboxInput("sieve_plot_checkbox", "Filter By Station", value = FALSE)
            ),
            conditionalPanel(
              condition = "input.sieve_plot_checkbox == true",
              selectInput("sieve_plot_station", "Select Station:", choices = NULL)
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
            tabPanel("Autoplot", mainPanel(plotOutput("pca_plot"))),
            tabPanel("Scree Plot", mainPanel(plotOutput("scree_plot")))
          )
        )
      )
    )
  )  # Close tabsetPanel
)  # Close fluidPage
