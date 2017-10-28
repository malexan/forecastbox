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

  y / y * e


}
