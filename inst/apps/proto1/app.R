library(forecastbox)
library(forecast)
library(ggplot2)
library(lubridate)
library(markdown)
library(V8)
library(shiny)
library(shinyjs)
library(shinythemes)
library(purrr)
library(dplyr)

mdls <- c("ets", "bats", "auto.arima")

# UI ####
ui <- fluidPage(
  useShinyjs(),
  tags$head(includeHTML("google-analytics.html")),
  theme = shinytheme("superhero"),

  titlePanel("Forecast for one time series (Alpha version)"),

  tabsetPanel(
    # Overview tab ####
    tabPanel(
      "Overview",
      p(paste0("Application version: ", packageVersion("forecastbox"))),
      p(paste0("Built: ",
               stringr::str_split(
                 packageDescription("forecastbox",
                                    fields = c("Built"),
                                    drop = T), ";")[[1]][3])),
      includeMarkdown("intro_en.md")
    ),
    # Data input tab ####
    tabPanel(
      "Data input",

      sidebarLayout(
        sidebarPanel(
          radioButtons("data_source",
                       "What dataset to use",
                       c("User dataset" = "user",
                         "Built-in dataset: gas production in AU" = "gas")),
          div(id = "user_data_props",
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

    # tabPanel Check TS ####
    tabPanel("Check the time series",
             plotOutput("tsplot"),
             plotOutput("acfplot"),
             plotOutput("seasonplot"),
             plotOutput("seasonalplotpolar"),
             plotOutput("subseriesplot"),
             plotOutput("lagplot")
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
                      max = 12,
                      value = 6,
                      round = TRUE,
                      step = 1),
          sliderInput("cnfdnc_intrvl1",
                      "Confidence level 1",
                      min = 60,
                      max = 99,
                      value = 80,
                      step = 1,
                      post = "%"),
          sliderInput("cnfdnc_intrvl2",
                      "Confidence level 2",
                      min = 60,
                      max = 99,
                      value = 95,
                      step = 1,
                      post = "%")
        ),

        mainPanel(

          plotOutput("forecastPlot"),
          tableOutput("forecastData")
        )
      )
    )
  )
)

server <- function(input, output) {

  # Disable data input if user prefers built-in dataset
  observe({
    shinyjs::toggleState(
      condition = input$data_source == "user",
      selector = "div[id^='user_data']")
  })

  tsdata <- reactive(
    {
      # Validation of input ####
      validate(
        need(
          stringr::str_detect(input$rawdata, "\\d"),
          "No digits in input field"),
        need(stringr::str_detect(
          input$rawdata,
          paste0(
            "^[\\d\\s",
            if_else(input$dec == ".", "\\.", input$dec),
            "]+$")),
          "Only digits, decimial point, space, new line, and tab are allowed in input field")
      )

      # Read raw data ####

      raw_column2tbl(raw_column = input$rawdata,
                     dec = input$dec,
                     startdate = input$startdate)
    }
  )

  # Convert to TS object ####
  ts1 <- reactive(
    if (input$data_source == "user") {
      convert_df2ts(tsdata(), select = "value",
                    freq = 12L)
    } else
      if (input$data_source == "gas") {
        forecast::gas
      } else stop("Unknown data set option")
  )

  # Generate TS data table ####
  output$tsdatatable <- renderTable(
    tsdata() %>%
      mutate(date = as.character(date)))

  # Raw data diagnostics plots ####
  output$tsplot <- renderPlot(autoplot(ts1()))
  output$acfplot <- renderPlot(
    ggAcf((ts1()))
  )

  output$seasonplot <- renderPlot({

    validate(
      need(length(ts1()) / frequency(ts1()) > 1.5,
           "Seasonal plot skipped: time series is too short.")
    )

    ggseasonplot(ts1(),
                 year.labels = TRUE,
                 year.labels.left = TRUE)
  }
  )

  output$seasonalplotpolar <- renderPlot({

     validate(
      need(length(ts1()) / frequency(ts1()) > 1.5,
           "Polar seasonal plot skipped: time series is too short.")
    )

     ggseasonplot(ts1(),
                 polar = TRUE)
  }
  )

  output$subseriesplot <- renderPlot({
     validate(
      need(length(ts1()) / frequency(ts1()) >= 2,
           "Subseries plot skipped: time series is too short.")
    )
    ggsubseriesplot(ts1())
  }
  )

  output$lagplot <- renderPlot({
    validate(
      need(length(ts1()) / frequency(ts1()) >= 2,
           "Lag plot skipped: time series is too short.")
    )
      gglagplot(ts1())

  })
  # Build forecast ####

  # Disable model selection if model to be selected automatically
  observe({
    shinyjs::toggleState("model", input$auto_slctn != TRUE)
  })

  frcst_mdl <- reactive(
    {
      if (input$auto_slctn != TRUE) {
        ftd_mdl <- fit_ts_mdl(ts1(), model = input$model)
      } else {
        bst_mdl <- best_mdl(
          ts1(),
          h = 1,
          tail2CV = 3,
          models = mdls,
          params = map(mdls,
                       frcst_fit_params))

        ftd_mdl <- fit_ts_mdl(ts1(), model = bst_mdl)
      }

      ftd_mdl
    }
  )

  frcst <- reactive({
    forecast::forecast(frcst_mdl(),
                       h = input$horizon,
                       level = c(input$cnfdnc_intrvl1,
                                 input$cnfdnc_intrvl2))
  }
  )

  frcst_tbl <- reactive(
    {
      sweep::sw_sweep(frcst(),
                      timekit_idx = TRUE,
                      rename_index = "date") %>%
        filter(key == "forecast") %>%
        select(-key) %>%
        mutate(date = zoo::as.Date.yearmon(date),
               date = as.character(date))
    }

  )

   output$forecastPlot <- renderPlot({
      plot(frcst())
   })

   output$forecastData <- renderTable(frcst_tbl())

}

# Run the application
shinyApp(ui = ui, server = server)

