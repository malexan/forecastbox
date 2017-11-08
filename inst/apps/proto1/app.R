library(forecastbox)
library(forecast)
library(ggplot2)
library(lubridate)
library(markdown)
library(V8)
library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)


ui <- fluidPage(
  theme = shinytheme("superhero"),

  titlePanel("Forecast for one time series (Alpha version)"),
  tabsetPanel(
    tabPanel(
      "Overview",
      includeMarkdown("intro_en.md")
    ),
    tabPanel(
      "Data input",

      sidebarLayout(
        sidebarPanel(
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
        ),
        mainPanel(
          fluidRow(
            column(3,
                   textAreaInput(
                     "rawdata",
                     "Paste data here:",
                     width = "150px",
                     height = "500px",
                     value = "")
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
          sliderInput("horizon",
                      "Number of periods to forecast:",
                      min = 1,
                      max = 12,
                      value = 6,
                      round = TRUE,
                      step = 1),
          radioButtons("model",
                       "Model to use:",
                       c("ARIMA" = "auto.arima",
                         "Exponential smoothing" = "ets"))
        ),

        mainPanel(

          plotOutput("forecastPlot"),
          tableOutput("forecastData")
        )
      )
    )
  )
)

# SERVER ####
server <- function(input, output) {
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
    convert_df2ts(tsdata(), select = "value",
                  freq = 12L)
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
  frcst_mdl <- reactive(
    {
      fit_ts_mdl(ts1(), model = input$model)
    }
  )

  frcst <- reactive({
    forecast::forecast(frcst_mdl(), h = input$horizon)
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

