shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("n", "Количество групп", 5, 100, 20)),
        mainPanel(plotOutput("hist"))
      )
    ),
    server = function(input, output) {
      output$hist <- renderPlot(
        hist(faithful$eruptions, breaks = input$n,
             col = "skyblue", border = "white",
             main = "Продолжительность извержений гейзера, м")
      )
    }
  )