#' Функция запуска Shiny-приложения для загрузки и обработки Excel файла
#'
#' @param browser Запускать ли браузер (логическая, TRUE по умолчанию)
#' @param host С каких компьютеров принимать запросы. По умолчанию со всех ("0.0.0.0"). Чтобы ограничить только локальной машиной, установите значение "127.0.0.1".
#'
#' @import readxl
#' @import shiny
#' @export
#'

forecastbox <- function(browser = TRUE, host = "0.0.0.0") {
  shiny::runApp(
    appDir = system.file("apps", "forecastbox", package = "forecastbox"),
    launch.browser = browser)
}
