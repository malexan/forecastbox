#' Fits forecast model based on time series, forecasting function and its
#' parameters.
#'
#' @param ts Time series object.
#' @param model Time series model function.
#' @param params List of parameters for the model function. If skipped than it
#'   is taken from \code{frcst_fit_params} function.
#'
#' @return Time series model object \code{ets}, \code{auto.arima}, etc.
#' @export

fit_ts_mdl <- function(ts = NULL, model = NULL, params = NULL) {

  stopifnot(!is.null(ts))
  stopifnot(is.ts(ts))
  stopifnot(is.character(model))

  if (is.null(params)) {
    params <- frcst_fit_params(model)
  }

  fit <- do.call(eval(parse(text = paste0("forecast::", model))),
                 args = c(list(y = ts), params))

  fit
}

