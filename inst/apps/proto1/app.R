library(shiny)

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
         plotOutput("forecastPlot")
      )
   )
)

server <- function(input, output) {

   output$forecastPlot <- renderPlot({

      fit <- forecast::bats(USAccDeaths)
      frcst <- forecast::forecast(fit, h = input$horizon)

      plot(frcst)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

