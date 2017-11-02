#' Time series cross-validation
#'
#' Computes the forecast errors obtained by applying forecastfunction to subsets
#' of the time series y using a rolling (accumulating) forecast origin. It
#' differs from forecast::tsCV by skipping head of the time series.
#'
#' @param y Time series object.
#' @param forecastfunction Forecasting function. It must be able take ts object as first parameter and h (forecasting horizon) as second parameter.
#' @param h Forecast horizon.
#' @param tail2CV Integer. It specifies number of time series elements for those predicitions will be build.
#' @param ... Additional parameters to forecastfunction.
#'
#' @return A time series object with time index identical to y and containing
#'   absolute prediction errors.
#'
#' @import forecast
#' @import purrr
#' @export


crossv_ts <- function(y, forecastfunction, h = 1, tail2CV = NULL, ...) {

  stopifnot(is_function(forecastfunction))
  stopifnot(is.ts(y))
  n <- length(y)
  # If no tail2CV than simply skip first sample
  # as it is not possible to build a forecast for it
  if (is.null(tail2CV)) tail2CV <- n - 1
  stopifnot(is.numeric(tail2CV))

  stopifnot(tail2CV < n)

  possible_frcst <- purrr::possibly(
    function(ts, h = h,
             tsend = tsend, ...) {
      ts_short <- subset(ts, end = tsend)
      forecast_fit <- forecastfunction(ts_short, h = h, ...)
      assert_that("forecast" %in% class(forecast_fit))
      # Calculate error
      ts[tsend + h] - forecast_fit$mean[h]
    }, NA_real_)

  head_skip_n <- n - tail2CV

  e <- map_dbl(seq.int(from = n - tail2CV, to = n - h),
               ~ possible_frcst(y, h = h, tsend = .x)
  )

  # Add first missing elements

  e <- c(rep(NA_real_, times = head_skip_n + h - 1), e)

  # Convert vector to time series object with time index identical to
  # the original time series
  y / y * e


}
