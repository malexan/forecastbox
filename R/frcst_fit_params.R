#' Returns parameters for fitting of selected forecasting model
#'
#' @param model "auto.arima", "ets" or "bats".
#' @return List with parameters.
#' @export

frcst_fit_params <- function(model = NULL) {

  stopifnot(!is.null(model))

  mdls <- list(
      auto.arima = list(stepwise = FALSE, approximation = FALSE),
      ets = list(),
      bats = list()
      )

  stopifnot(model %in% names(mdls))

  mdls[[model]]

}
