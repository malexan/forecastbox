library(forecastbox)
library(forecast)
library(dygraphs)
library(ggplot2)
library(lubridate)
library(markdown)
library(V8)
library(shiny)
library(shinyjs)
library(purrr)
library(dplyr)

mdls <- c("ets", "bats", "auto.arima")

# UI ####
ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  useShinyjs(),

  titlePanel("Forecast for one time series (Alpha version)"),

  ui_tabset_panel_main()
)

# Server ####
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
                       level = input$cnfdnc_intrvl)
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

   output$forecastPlot <- renderDygraph({
     cbind(history = frcst()$x,
           forecast = frcst()$mean,
           upper = frcst()$upper,
           lower = frcst()$lower) %>%
       dygraph()  %>%
       dyAxis("x", drawGrid = FALSE) %>%
       dySeries("history", label = "History") %>%
       dySeries(c("lower", "forecast", "upper"), label = "Forecast") %>%
       dyRangeSelector()
   })

   output$forecastData <- renderTable(frcst_tbl())

}

# Run the application
shinyApp(ui = ui, server = server)

