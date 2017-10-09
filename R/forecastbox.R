#' Функция запуска Shiny-приложения для загрузки и обработки Excel файла
#'
#' @param port TCP порт, который слушает приложение. Если порт не задан, и нет опции shiny.port, то будет использован случайный порт.
#' @param browser Запускать ли браузер (логическая, TRUE по умолчанию)
#' @param host С каких компьютеров принимать запросы. По умолчанию со всех ("0.0.0.0"). Чтобы ограничить только локальной машиной, установите значение "127.0.0.1".
#'
#' @import shiny
#' @import dplyr
#' @export
#'

forecastbox <- function(port = getOption("shiny.port"),
                        browser = TRUE,
                        host = "0.0.0.0") {
  shiny::runApp(
    appDir = system.file("apps", "forecastbox", package = "forecastbox"),
    port = port,
    launch.browser = browser,
    host = host)
}
