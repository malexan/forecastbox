#' Simple function with Shiny app inside
#'
#' @import shiny
#' @export

binner <- function() {
    shiny::runApp(system.file("apps", "binner", package = "forecastbox"))
}
