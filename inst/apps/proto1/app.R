library(shiny)
library(dplyr)

ui <- fluidPage(

   # Application title
   titlePanel("Forecast for one time series"),

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

server <- function(input, output) {

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

