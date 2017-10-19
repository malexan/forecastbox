library(forecastbox)
library(forecast)
library(ggplot2)
library(shiny)
library(tibble)
library(lubridate)
library(dplyr)


ui <- fluidPage(

  titlePanel("Forecast for one time series"),
  tabsetPanel(
    tabPanel(
      "Data input",
      helpText(
        "1. Paste a column with data from Excel.",
        br(),
        "2. Choose year and month for the first observation.",
        br(),
        "3. Check that the data is imported correctly.",
        br(),
        "4. Switch to Forecast panel and view results."),
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
    tabPanel("Check the time series",
             plotOutput("tsplot")
             ),
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          sliderInput("horizon",
                      "Number of periods to forecast:",
                      min = 1,
                      max = 12,
                      value = 3,
                      round = TRUE,
                      step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(

          plotOutput("forecastPlot"),
          tableOutput("forecastData")
        )
      )
    )
  )
)

server <- function(input, output) {

  tsdata <- reactive(
    {

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
      rawdata <- read.table(textConnection(input$rawdata),
                            header = FALSE,
                            dec = input$dec)
      rawdata <- rawdata[[1]]
      timevec <- seq.Date(
        from = input$startdate,
        by = input$timeunit,
        along.with = rawdata
      )
      tibble(date = timevec,
             value = rawdata)
    }
  )

  ts1 <- reactive(
    convert_df2ts(tsdata(), select = "value",
                  freq = 12L)
  )

  output$tsdatatable <- renderTable(
    tsdata() %>%
      mutate(date = as.character(date)))

  output$tsplot <- renderPlot(autoplot(ts1()))

  frcst <- reactive(
    {
      fit <- forecast::auto.arima(ts1())
      forecast::forecast(fit, h = input$horizon)
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

