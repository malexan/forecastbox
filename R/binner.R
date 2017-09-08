#' Simple function with Shiny app inside
#'
#' Простая функция для тестирования установки под Windows.
#'
#' @import shiny
#' @export

binner <- function() {
    shiny::runApp(system.file("apps", "binner", package = "forecastbox"))
}
