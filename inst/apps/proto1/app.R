library(forecastbox)
library(shiny)
library(tibble)
library(lubridate)
library(dplyr)


ui <- fluidPage(

  # Application title
  titlePanel("Forecast for one time series"),
  tabsetPanel(
    tabPanel(
      "inputtable",
      sidebarLayout(
        sidebarPanel(
          dateInput("startdate",
                    "Start Date",
                    weekstart = 1,
                    width = "200px"),
          radioButtons("timeunit",
                       "Time series unit",
                       c("Day" = "day",
                         "Week" = "week",
                         "Month" = "month",
                         "Quarter" = "quarter",
                         "Year" = "year"),
                       selected = "month"),
          radioButtons("season",
                       "Main season duration",
                       c("Week" = "week",
                         "Month" = "month",
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
                     "Replace numbers with your data",
                     width = "150px",
                     height = "500px",
                     value = "1\n2\n3")
            ),
            column(5,
                   tableOutput("tsdatatable")
            )
          )
        )
      )
    ),
    tabPanel(
      "results",
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
        need(stringr::str_detect(
          input$rawdata,
          paste0(
            "^[\\d\\s",
            if_else(input$dec == ".", "\\.", input$dec),
            "]+$")),
          "Only digits, decimial point, space, new line, and tab are allowed in input field"),
        need(
          stringr::str_detect(input$rawdata, "\\d"),
          "No digits in input field")
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

  output$tsdatatable <- renderTable(
    tsdata() %>%
      mutate(date = as.character(date)))

  frcst <- reactive(
    {
      fit <- forecast::bats(USAccDeaths)
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

