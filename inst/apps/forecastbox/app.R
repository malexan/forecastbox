library("dplyr")


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
      mainPanel(
        tableOutput("contents")
      )
    )
  ),
  server = function(input, output) {
    output$contents <- renderTable({

      excel <- input$excel

      if (is.null(excel))
        return(NULL)

      forecastbox:::import_excel(excel$datapath) %>%
        forecastbox:::convert_data() %>%
        forecastbox:::cal_adj() %>%
        forecastbox:::run_predict(samplesize = 10L) %>%
        forecastbox:::cal_adj_back()

    })
  }
)
