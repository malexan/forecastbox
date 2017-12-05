#' Login UI page
#'
#' @import shiny
#' @export
#'

ui_login <- function() {
  fluidRow(column(width=4, offset = 4,
                  wellPanel(id = "login",
                            textInput(".username", "Username (test):", "test"),
                            passwordInput(".password", "Password (123):", "123"),
                            div(actionButton(".login", "Log in"), style="text-align: center;")
                  ),
                  textOutput("message")
  ))

}
