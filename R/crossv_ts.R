#' Time series cross-validation
#'
#' Computes the forecast errors obtained by applying forecastfunction to subsets
#' of the time series y using a rolling forecast origin. It differs from
#' forecast::tsCV by skipping head of the time series, when it is too shot.
#'
#' @param y Time series object.
#' @param forecastfunction Forecasting function. It must be able take ts object as first parameter and h (forecasting horizon) as second parameter.
#' @param tail2CV Integer. It specifies number of time series elements for those predicitions will be build.
#' @param ... Additional parameters to forecastfunction.
#'
#' @return Time series with error.
#'
#' @import forecast
#' @import purrr


crossv_ts <- function(y, forecastfunction, h = 1, tail2CV = NULL, ...) {

  stopifnot(is_function(forecastfunction))
  stopifnot(is.ts(y))
  n <- length(y)
  if (is.null(tail2CV)) tail2CV <- n - 1
  stopifnot(is.numeric(tail2CV))

  stopifnot(tail2CV < n)

  possible_frcst_err <- purrr::possibly(
    function(ts, h = h,
             tsend = tsend, ...) {
      ts_short <- subset(ts, end = tsend)
      fc <- forecastfunction(ts_short, h = h, ...)
      ts[tsend + h] - fc$mean[h]
    }, NA_real_)

  head_skip_n <- n - tail2CV

  e <- map_dbl(seq.int(from = n - tail2CV - h + 1, to = n - h),
               ~ possible_frcst_err(y, h = h, tsend = .x)
  )

  # Add first missing elements

  e <- c(rep(NA_real_, times = head_skip_n), e)

  # What was it? Convert vector to time series object?

  y / y * e


}
