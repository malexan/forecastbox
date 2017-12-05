#' Tabset panel for UI
#'
#' @import shiny
#' @export


ui_tabset_panel_main <- function() {
  tabsetPanel(
    # tabPanel Data input ####
    tabPanel(
      "Data input",

      sidebarLayout(
        sidebarPanel(
          radioButtons("data_source",
                       "Dataset to use",
                       c("User dataset" = "user",
                         "Built-in dataset: gas production in AU" = "gas")),
          div(id = "user_data_props", # Group by id to make disabling easier
              dateInput("startdate",
                        "Date of first observation",
                        weekstart = 1,
                        startview = "year",
                        width = "200px"),
              radioButtons("timeunit",
                           "Time series unit",
                           c(# "Day" = "day",
                             # "Week" = "week",
                             "Month" = "month" # ,
                             # "Quarter" = "quarter",
                             #"Year" = "year"
                           ),
                           selected = "month"),
              radioButtons("season",
                           "Main season duration",
                           c(# "Week" = "week",
                             # "Month" = "month",
                             "Year" = "year"),
                           selected = "year"
              ),
              radioButtons("dec",
                           "Decimal point",
                           c("." = ".",
                             "," = ","),
                           width = "50px")
          )
        ),
        mainPanel(
          fluidRow(
            column(3,
                   div(id = "user_data_input",
                       textAreaInput(
                         "rawdata",
                         "Paste data here:",
                         width = "150px",
                         height = "500px",
                         value = "")
                   )
            ),
            column(5,
                   tableOutput("tsdatatable")
            )
          )
        )
      )
    ),

    # tabPanel Forecast ####
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          radioButtons("auto_slctn",
                       "How to choose model",
                       c("Manually" = FALSE,
                         "Auto (smallest mean error)" = TRUE)),
          radioButtons("model",
                       "Model to use:",
                       c("Auto ARIMA" = "auto.arima",
                         "Exponential smoothing" = "ets")),
          sliderInput("horizon",
                      "Number of periods to forecast:",
                      min = 1,
                      max = 36,
                      value = 24,
                      round = TRUE,
                      step = 1),
          sliderInput("cnfdnc_intrvl",
                      "Confidence level",
                      min = 60,
                      max = 99,
                      value = 80,
                      step = 1,
                      post = "%")
        ),

        mainPanel(

          dygraphOutput("forecastPlot"),
          tableOutput("forecastData")
        )
      )
    ),

    # tabPanel Check TS ####
    tabPanel("Advanced: check the time series",
             plotOutput("tsplot"),
             plotOutput("acfplot"),
             plotOutput("seasonplot"),
             plotOutput("seasonalplotpolar"),
             plotOutput("subseriesplot"),
             plotOutput("lagplot")
    ),

    # tabPanel Help ####
    tabPanel(
      "Help",
      p(paste0("Application version: ", packageVersion("forecastbox"))),
      p(paste0("Built: ",
               stringr::str_split(
                 packageDescription("forecastbox",
                                    fields = c("Built"),
                                    drop = T), ";")[[1]][3])),
      includeMarkdown("intro_en.md")
    )
  )
}
