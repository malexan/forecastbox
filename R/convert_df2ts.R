#' Converts data frame to ts object. Only monthly data is supported.
#'
#' @param data Data frame
#' @import assertthat
#' @export


convert_df2ts <- function(data, select = NULL, freq = 12L) {

  data <- tibble::as.tibble(data)

  datecol <- timetk::tk_get_timeseries_variables(data)

  assert_that(is.string(datecol))

  start_moment <- min(data[[datecol]])
  year_min <- lubridate::year(start_moment)
  month_min <- lubridate::month(start_moment)

  end_moment <- max(data[[datecol]])
  year_max <- lubridate::year(end_moment)
  month_max <- lubridate::month(end_moment)

  timetk::tk_ts_(data,
                 select = select,
                 start = c(year_min, month_min),
                 end = c(year_max, month_max),
                 frequency = freq
  )

}
