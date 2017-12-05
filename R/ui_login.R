#' Login UI page
#'
#' @import shiny
#' @export
#'

ui_login <- function() {
  fluidRow(column(width=4, offset = 4,
                  wellPanel(id = "login",
                            textInput(".username", "Username:"),
                            passwordInput(".password", "Password:"),
                            div(actionButton(".login", "Log in"), style="text-align: center;")
                  ),
                  textOutput("message")
  ))

}
