shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput(
          inputId = "excel",
          label = "Загрузите файл Excel",
          accept = c(".xls", ".xlsx"),
          buttonLabel = "Выбрать...",
          placeholder = "Файл не выбран")
      ),
      mainPanel(plotOutput("hist"))
    )
  ),
  server = function(input, output) {
    output$hist <- renderPlot(
      hist(faithful$eruptions, breaks = 30L,
           col = "skyblue", border = "white",
           main = "Продолжительность извержений гейзера, м")
    )
  }
)
