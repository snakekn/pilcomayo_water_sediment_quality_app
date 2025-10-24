# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* small styling for the selector box (tweak as needed) */
    #scope-selector { margin: 8px 12px; padding: 8px; border-radius: 6px; background:#f8f9fa; display:inline-block; }
  "))),
  
  tabsetPanel(id = "main_tab",
              
              tabPanel("Introduction",
                       fluidPage(
                         titlePanel("Sediment & Water Quality in the Pilcomayo River Basin"),
                         tags$hr(),
                         tags$img(src = "pilcomayo.jpg", height = "350px"),
                         tags$hr(),
                         includeMarkdown("text/introduction.md"), # load from a .md to reduce clutter
                         tags$hr(),
                         tags$h4("Download Data:"),
                         
                         radioButtons(
                           inputId = "data_scope",
                           label = "Select Data Scope:",
                           choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
                           selected = "bol",  # or "all" if you want to default to everything
                           inline = TRUE
                         ),
                         
                         # Optional year selection
                         fluidRow(
                           column(6,
                                  uiOutput("download_year_ui")
                           )
                         ),
                         
                         # Download buttons
                         fluidRow(
                           column(6, downloadButton("download_sed_clean", "Sediment Data (Clean)")),
                           column(6, downloadButton("download_water_clean", "Water Data (Clean)")),
                           
                         ),
                         tags$p(),
                         fluidRow(
                           column(6, downloadButton("download_sed_usgs", "Sediment Data (Compared to SQGs)")),
                           column(6, downloadButton("download_water_1333", "Water Data (Compared to Bolivian Standards)"))
                         ),
                         tags$p(),
                         fluidRow(
                           column(6, downloadButton("download_usgs_standards", "USGS Sediment Quality Guidelines Table")),
                           column(6, downloadButton("download_1333_standards", "Bolivian Ley No. 1333 Standards Table"))
                         ),
                         
                         tags$hr(),
                         
                         tabPanel("Import", dataUploadUI("import")),  # <— drop-in
                         
                         tags$hr(),
                         tags$hr(),
                         includeMarkdown("text/introduction_sources.md"),
                         tags$hr(),
                         tags$p("This application was developed using R Shiny and integrates spatial and tabular data for interactive analysis.")
                       )
              ),
              
              tabPanel("Sediment Map",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("sed_year", "Select Year:", choices = NULL),
                           uiOutput("sed_campaign_ui"),
                           uiOutput("tamiz_ui"),
                           selectInput("sed_metal", "Select Metal:", choices = NULL),
                           radioButtons("sed_value_type", "Symbolize by:",
                                        choices = c("Measured Concentration (mg/kg)" = "sed_value", "Compare to USGS SQGs" = "usgs"),
                                        selected = "sed_value"),
                           uiOutput("sed_legend"),
                           
                           # Descriptive text for sediment map
                           info_callout("Sediment Quality Map", "This map displays sediment quality parameters from monitoring campaigns. 
                                  Circle size represents the measured concentration, while colors can show either 
                                  raw values or comparison to USGS Sediment Quality Guidelines 
                                  (TEL/PEL thresholds). Data can be filtered by year, campaign, and sieve size. 
                                  Data is sourced from www2.pilcomayo.net.")
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Map", leafletOutput("sed_map", height = 600)),
                             tabPanel("Table", dataTableOutput("sed_table")),
                             tabPanel("USGS SQGs", dataTableOutput("stds_usgs_table"))
                           )
                         )
                       )
              ),
              
              # tabPanel("Water Map",
              #          sidebarLayout(
              #            sidebarPanel(
              #              selectInput("water_year", "Select Year:", choices = NULL),
              #              uiOutput("water_campaign_ui"),
              #              selectInput("water_metal", "Select Metal:", choices = NULL),
              #              radioButtons("water_value_type", "Symbolize by:",
              #                           choices = c("Dissolved Concentration (mg/l)" = "water_dissolved",
              #                                       "Suspended Concentration (mg/kg)" = "water_suspended",
              #                                       "Total Concentration (mg/l)" = "water_total",
              #                                       "Compare to Bolivian Standards" = "water_1333"),
              #                           selected = "water_total"),
              #              uiOutput("water_legend"),
              #            ),
              #            mainPanel(
              #              tabsetPanel(
              #                tabPanel("Map", leafletOutput("water_map", height = 600)),
              #                tabPanel("Table", dataTableOutput("water_table")),
              #                tabPanel("Bolivian Standards", dataTableOutput("stds_1333_table"))
              #              )
              #            )
              #          )
              # ),
              tabPanel("Water Maps",
                       sidebarLayout(
                         sidebarPanel(
                           # Common slider (will be dynamically updated based on active tab)
                           uiOutput("map_date_slider_ui"),
                           
                           # Conditional panels for different controls based on active tab
                           conditionalPanel(
                             condition = "input.map_tabs == 'parameter_map'",
                             uiOutput("parameter_selector_ui")
                           ),
                           
                           conditionalPanel(
                             condition = "input.map_tabs == 'classification_map'",
                             uiOutput("metal_selector_ui")
                           ),
                           
                           # Descriptive text for each map
                           conditionalPanel(
                             condition = "input.map_tabs == 'parameter_map'",
                             info_callout("Parameter Map", "This map displays water quality parameters using binned color scaling. 
                                    Circle size represents the parameter value, while colors show the 
                                    range using a logarithmic scale. Clicking the ▶ button will start an animation showing change over time. 
                                    Data is sourced from www2.pilcomayo.net.")
                           ),
                           
                           conditionalPanel(
                             condition = "input.map_tabs == 'classification_map'",
                             info_callout("Classification Map", text = "This map displays water quality parameters using discrete color classes 
                                    (A, B, C, D, Unclassified). These classes are based on standards from Bolivian Ley No. 1333. 
                                    Circle colors represent the classification level, 
                                    while size indicates the actual measured value. Clicking the ▶ button will start an animation showing change over time.
                                    Data is sourced from www2.pilcomayo.net.")
                           )
                         ),
                         
                         mainPanel(
                           tabsetPanel(id = "map_tabs",
                                       tabPanel("Classification Map", 
                                                value = "classification_map",
                                                leafletOutput("classification_timeline_map", height = 600)
                                       ),
                                       tabPanel("Measured Value Map", 
                                                value = "parameter_map",
                                                leafletOutput("parameter_timeline_map", height = 600)
                                       ),
                                       tabPanel("Bolivian Standards", dataTableOutput("stds_1333_table"))
                           )
                         )
                       )
              ),
              tabPanel("Time Series",
                       sidebarLayout(
                         sidebarPanel(
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
                           
                           # Descriptive text for each time series tab
                           conditionalPanel(
                             condition = "input.ts_tabs == 'Water Samples'",
                             info_callout("Water Quality Time Series", "This plot displays the temporal variation of water quality parameters 
                                    at the selected monitoring station. Points represent individual measurements, 
                                    while the line shows the trend over time. Reference standards from Bolivian Ley No. 1333 
                                    are shown as dashed horizontal lines (where applicable). Data is sourced from www2.pilcomayo.net.")
                           ),
                           
                           conditionalPanel(
                             condition = "input.ts_tabs == 'Sediment Samples'",
                             info_callout("Sediment Quality Time Series", "This plot displays the temporal variation of sediment quality parameters 
                                    at the selected monitoring station. Individual points show measurements, 
                                    with the connecting line representing daily averages when multiple samples 
                                    exist per date. Darker points represent samples taken further from the river bank (when variable). 
                                    USGS Sediment Quality Guidelines (TEL/PEL) are shown as dashed horizontal lines (where applicable). 
                                    Data is sourced from www2.pilcomayo.net.")
                           )
                         ),
                         mainPanel(
                           tabsetPanel(
                             id = "ts_tabs",
                             tabPanel("Water Samples", plotlyOutput("ts_plot_water")),
                             tabPanel("Sediment Samples", plotlyOutput("ts_plot_sed")),
                             tabPanel("Bolivian Standards", dataTableOutput("stds_1333_table_ts")),
                             tabPanel("USGS SQGs", dataTableOutput("stds_usgs_table_ts"))
                           )
                         )
                       )
              ),
              tabPanel("Ranking Plots",
                       
                       tabsetPanel(
                         tabPanel("Worst Observations", 
                                  fluidRow(
                                    column(4,
                                           radioButtons("observation_plot_type", "Rank Observations Using:",
                                                        choices = c("Raw Water Samples" = "value",
                                                                    "Bolivian Water Standards" = "class",
                                                                    "Raw Sediment Samples" = "sed_value",
                                                                    "USGS SQGs" = "usgs"
                                                        )
                                           ),
                                           conditionalPanel(condition = "input.observation_plot_type == 'class'",
                                                            radioButtons("observation_plot_class", "Rank by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "# Unclassified Parameters" = "unclassified",
                                                                                     "# Class D Parameters" = "class_d",
                                                                                     "# Class C Parameters" = "class_c",
                                                                                     "# Class B Parameters" = "class_b"))
                                           ),
                                           conditionalPanel(condition = "input.observation_plot_type == 'value'",
                                                            selectInput("observation_plot_param", "Select Parameter:", choices = NULL),
                                           ),
                                           conditionalPanel(condition = "input.observation_plot_type == 'usgs'",
                                                            radioButtons("observation_plot_usgs", "Rank by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "# Parameters Above PEL" = "above_pel",
                                                                                     "# Parameters Above TEL" = "above_tel"
                                                                         )
                                                            )),
                                           conditionalPanel(condition = "input.observation_plot_type == 'sed_value'",
                                                            selectInput("observation_plot_param_sed", "Select Parameter:", 
                                                                        choices = NULL)),
                                           # Caption added here
                                           conditionalPanel(
                                             condition = "input.observation_plot_type == 'class'",
                                             info_callout("Observation Ranking", "This plot ranks individual water samples based on water quality standards from Bolivian law.
                                                    Observations can be ranked by the number of parameters that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each observation.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           # Caption added here
                                           conditionalPanel(
                                             condition = "input.observation_plot_type == 'value'",
                                             info_callout("Observation Ranking", "This plot ranks individual water samples based on measured values of the selected water quality parameter.
                                                    For most parameters, higher values are ranked higher (worse). However, for some (DO, pH, etc.), lower values are ranked higher (worse).
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.observation_plot_type == 'usgs'",
                                             info_callout("Observation Ranking", "This plot ranks individual sediment samples based on USGS Sediment Quality Guidelines (SQGs).
                                                    Observations can be ranked by the number of parameters that fall into each category, or by overall score.
                                                      Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above Pel=2), and finding the mean value for each observation.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.observation_plot_type == 'sed_value'",
                                             info_callout("Observation Ranking", "This plot ranks individual sediment samples based on measured values of the selected sediment quality parameter.
                                                 Higher values are ranked worse.
                                                 Data is sourced from www2.pilcomayo.net.")
                                           )
                                    ),
                                    column(8, plotlyOutput("observation_scores_plot", height = "500px")))),
                         tabPanel("Worst Stations", 
                                  fluidRow(
                                    column(4,
                                           radioButtons("station_plot_type", "Rank Stations Using:",
                                                        choices = c(
                                                          "Raw Water Samples" = "value",
                                                          "Bolivian Water Standards" = "class",
                                                          "Raw Sediment Samples" = "sed_value",
                                                          "USGS SQGs" = "usgs"
                                                        )
                                           ),
                                           conditionalPanel(condition = "input.station_plot_type == 'class'",
                                                            radioButtons("station_plot_class", "Rank by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "Mean # Unclassified Parameters" = "unclassified",
                                                                                     "Mean # Class D Parameters" = "class_d",
                                                                                     "Mean # Class C Parameters" = "class_c",
                                                                                     "Mean # Class B Parameters" = "class_b"))
                                           ),
                                           conditionalPanel(condition = "input.station_plot_type == 'class' && input.station_plot_class == 'worst_score'",
                                                            checkboxInput("station_plot_recency", "Weigh recent observations higher", value = FALSE)),
                                           conditionalPanel(condition = "input.station_plot_type == 'value'",
                                                            selectInput("station_plot_param", "Select Parameter:", choices = NULL),
                                           ),
                                           conditionalPanel(condition = "input.station_plot_type == 'usgs'",
                                                            radioButtons("station_plot_usgs", "Rank by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "Mean # Parameters Above PEL" = "above_pel",
                                                                                     "Mean # Parameters Above TEL" = "above_tel"))),
                                           conditionalPanel(condition = "input.station_plot_type == 'usgs' && input.station_plot_usgs == 'worst_score'",
                                                            checkboxInput("station_plot_recency_sed", "Weigh recent observations higher", value = FALSE)),
                                           conditionalPanel(condition = "input.station_plot_type == 'sed_value'",
                                                            selectInput("station_plot_param_sed", "Select Parameter:",
                                                                        choices = NULL)),
                                           conditionalPanel(condition = "input.station_plot_type == 'value' | input.station_plot_type == 'sed_value'",
                                                            radioButtons("station_param_type", "Rank by:", 
                                                                         choices = c(
                                                                           "Average Value" = "avg",
                                                                           "Most Extreme Value" = "max"
                                                                         ))),
                                           # Caption added here
                                           conditionalPanel(
                                             condition = "input.station_plot_type == 'class'",
                                             info_callout("Station Ranking", "This plot ranks water sampling stations based on water quality standards from Bolivian law.
                                                    Stations can be ranked by the mean number of parameters that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each station.
                                                      For Overall Score, weighted mean can be used instead to emphasize recent observations (weight = 1 / (1 + years since present)). 
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.station_plot_type == 'value'",
                                             info_callout("Station Ranking", "This plot ranks water sampling stations based on measured values of the selected parameter.
                                                    For most parameters, higher values are ranked higher (worse). However, for some (DO, pH, etc.), lower values are ranked higher (worse).
                                                      Stations can be ranked based on the most extreme/worst recorded value, or the mean value across observations at that station.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.station_plot_type == 'usgs'",
                                             info_callout("Station Ranking", "This plot ranks sediment sampling stations based on USGS Sediment Quality Guidelines (SQGs).
                                                    Stations can be ranked by the mean number of parameters that fall into each category, or by overall score.
                                                      Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above PEL=2), and finding the mean value for each station.
                                                      For Overall Score, weighted mean can be used instead to emphasize recent observations (weight = 1 / (1 + years since present)). 
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.station_plot_type == 'sed_value'",
                                             info_callout("Station Ranking", "This plot ranks sediment sampling stations based on measured values of the selected parameter.
                                                    Higher values are ranked worse.
                                                      Stations can be ranked based on the most highest recorded value, or the mean value across observations at that station.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           )
                                    ),
                                    column(8, plotlyOutput("station_scores_plot", height = "500px")))),
                         tabPanel("Worst Parameters",
                                  fluidRow(
                                    column(4,
                                           radioButtons("param_plot_type", "Rank Parameters Using:",
                                                        choices = c(
                                                          "Bolivian Water Standards" = "class",
                                                          "USGS SQGs" = "usgs"
                                                        )
                                           ),
                                           conditionalPanel(condition = "input.param_plot_type == 'class'",
                                                            radioButtons("param_plot_class", "Rank by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "% Observations Unclassified" = "unclassified",
                                                                                     "% Observations in Class D" = "class_d",
                                                                                     "% Observations in Class C" = "class_c",
                                                                                     "% Observations in Class B" = "class_b"
                                                                         ))),
                                           conditionalPanel(condition = "input.param_plot_type == 'usgs'",
                                                            radioButtons("param_plot_usgs", "Rank by Observations by:",
                                                                         choices = c("Worst Overall Score" = "worst_score",
                                                                                     "% Observations Above PEL" = "above_pel",
                                                                                     "% Observations Above TEL" = "above_tel"
                                                                         ))),
                                           checkboxInput("param_plot_checkbox", "Filter by Station", value = FALSE),
                                           conditionalPanel(condition = "input.param_plot_checkbox == true",
                                                            selectInput("param_plot_station", "Select Station:",
                                                                        choices = NULL)),
                                           # Caption added here
                                           conditionalPanel(
                                             condition = "input.param_plot_type == 'class'",  
                                             info_callout("Parameter Ranking", "This plot ranks water quality parameters based on standards from Bolivian law.
                                                    Parameters can be ranked by the percent of observations that fall into each classification, or by overall score.
                                                      Overall score is calculated by assigning values to each classification (A=0 to Unclassified=4), and finding the mean value for each parameter.
                                                      Light bars represent percents/scores calculated after omitting NA rows for that parameter.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           ),
                                           conditionalPanel(
                                             condition = "input.param_plot_type == 'usgs'",  
                                             info_callout("Parameter Ranking", "This plot ranks sediment quality parameters based on USGS Sediment Quality Guidelines (SQGs).
                                                    Parameters can be ranked by the percent of observations that fall into each category, or by overall score.
                                                      Overall score is calculated by assigning values to each category (Below TEL=0, Above TEL=1, Above PEL=2), and finding the mean value for each parameter.
                                                      Light bars represent percents/scores calculated after omitting NA rows for that parameter.
                                                      Data is sourced from www2.pilcomayo.net.")
                                           )
                                    ),
                                    column(8,
                                           plotlyOutput("param_scores_plot", height = "500px")
                                    )
                                  )
                         ),
                         tabPanel("Worst Sieve Sizes",
                                  fluidRow(
                                    column(4,
                                           radioButtons("sieve_plot_type", "Rank Sieve Sizes Using:",
                                                        choices = c("Raw Sediment Samples" = "sed_value",
                                                                    "USGS SQGs" = "usgs")),
                                           conditionalPanel(condition = "input.sieve_plot_type == 'sed_value'",
                                                            selectInput("sieve_plot_param", "Select Parameter:", choices = NULL)),
                                           conditionalPanel(condition = "input.sieve_plot_type == 'sed_value'",
                                                            radioButtons("sieve_param_type", "Rank by:", 
                                                                         choices = c(
                                                                           "Average Value" = "avg",
                                                                           "Most Extreme Value" = "max"
                                                                         ))),
                                           conditionalPanel(condition = "input.sieve_plot_type == 'usgs'",
                                                            radioButtons("sieve_plot_usgs", "Rank By:",
                                                                         choices = c("Worst Scored Overall" = "worst_score",
                                                                                     "Mean # Observations Above PEL" = "above_pel",
                                                                                     "Mean # Observations Above TEL" = "above_tel"))),
                                           conditionalPanel(condition = "input.sieve_plot_type == 'usgs'",
                                                            checkboxInput("sieve_plot_checkbox", "Filter By Station", value = FALSE)),
                                           conditionalPanel(condition = "input.sieve_plot_checkbox == true",
                                                            selectInput("sieve_plot_station", "Select Station:",
                                                                        choices = NULL))
                                    ),
                                    column(8,
                                           plotlyOutput("sieve_scores_plot", height = "500px"))
                                  ))
                       )
                       
              ),
              tabPanel("Principal Component Analysis",
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("pca_parameters", "Select Parameters for PCA:",
                                          choices = NULL,
                                          multiple = TRUE,
                                          options = list(maxItems = 15)),
                           actionButton("deselect_all_pca", "Clear Selection"),
                           br(), br(),
                           actionButton("run_pca", "Run PCA", class = "btn-primary"),
                           
                           # Descriptive text for PCA
                           info_callout("Principal Component Analysis", "This analysis performs PCA on selected water quality parameters to identify 
                                    underlying patterns and relationships in the data. Missing values are 
                                    imputed using optimal component estimation. The variable plot shows parameter 
                                    contributions and correlations, colored by representation quality (cos²). 
                                    The scree plot displays variance explained by each component to help determine 
                                    the optimal number of dimensions. Select up to 15 parameters and click 'Run PCA' 
                                    to begin the analysis. Data is sourced from www2.pilcomayo.net.")
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Autoplot",
                                      mainPanel(
                                        plotOutput("pca_plot")
                                      )
                             ),
                             tabPanel("Scree Plot",
                                      mainPanel(
                                        plotOutput("scree_plot")
                                      ))
                           )
                         )
                         
                       )
              ),
              tabPanel("Regional Risk Analysis",
                       includeMarkdown("text/risk_analysis_about.md"),
                       leafletOutput("risk_map", height="600px"),
                       accordion(id="risk_analysis_factors", open=FALSE,
                                 accordion_panel("Weights for risk analysis calculation",
                                                 includeMarkdown("text/risk_analysis_weights.md"))
                       )
              )),
  ## 2) Put the conditionalPanel AFTER the tabsetPanel (so it won't be treated as a tab).
  ##    It's still outside the tabsetPanel (so no ghost tab), but the script below will move it
  ##    visually *into* the tab content area.
  div(id = "scope-selector-wrapper",
      conditionalPanel(
        condition = "input.main_tab != 'Introduction'",
        div(id = "scope-selector",
            radioButtons("plot_data_scope", "Data Scope:",
                         choices = c("Bolivia Only" = "bol", "All Locations" = "all"),
                         selected = "bol", inline = TRUE)
        )
      )
  ),
  
  ## 3) Move the wrapper into the tab content area so it appears under the tab headers (not above)
  tags$script(HTML("
  $(function(){
    function moveScope(){
      // find the tab-content corresponding to #main_tab
      var tabContent = $('#main_tab').closest('.container-fluid').find('.tab-content').first();
      var wrapper = $('#scope-selector-wrapper');
      if(tabContent.length && wrapper.length){
        // Prepend instead of append so it goes right under the tab headers
        if(wrapper.parent().get(0) !== tabContent.get(0)){
          tabContent.prepend(wrapper);
        }
      }
    }
    moveScope();
    setTimeout(moveScope, 250);
  });
"))
)