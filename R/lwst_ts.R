#' Selects time series with lowest average value
#'
#' @param ts_lst Named list with time series.
#' @return Name of the time series with lowest average absolute value.
#'
#' @import purrr
#' @export
#'

lwst_ts <- function(ts_lst) {

  all_ts <- ts_lst %>%
    map_dbl(~ median(abs(.x), na.rm = TRUE))

  names(all_ts)[all_ts == min(all_ts)]

}
