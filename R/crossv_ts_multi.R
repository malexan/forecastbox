#' Time series cross-validation for list of modelling functions
#'
#' Computes the forecast errors obtained by applying each of models to subsets
#' of the time series ts using a rolling (accumulating) forecast origin.
#'
#' @import forecast
#' @import purrr
#' @export
#'
#' @examples
#'
#' crossv_ts_multi(gas, h = 1,
#'                tail2CV = 6,
#'                models = c("auto.arima", "ets", "bats"),
#'                params = list(
#'                  list(stepwise = FALSE,
#'                       approximation = FALSE),
#'                  list(damped = TRUE),
#'                  list()
#'                )
#' )

crossv_ts_multi <- function(ts, h, tail2CV, models, params) {

  names(models) <- models
  # Convert character to expression
  models <- map(models, ~ eval(parse(text = .x)))
  # Wrap modelling function to forecasting function
  models <- map(models, ~ function(ts, h) forecast(.x(ts), h = h))

  map2(models, params, ~ crossv_ts(ts, .x, h = h, tail2CV = tail2CV, ... = .y))

}
