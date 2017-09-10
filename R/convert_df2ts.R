#' Converts data frame to ts object.
#'
#'@import assertthat


convert_df2ts <- function(data, select = NULL, freq = 12L) {

  data <- tibble::as.tibble(data)

  datecol <- colnames(data)[purrr::map_lgl(data,
                                           ~ "Date" %in% class(.x))]

  assert_that(is.string(datecol))

  start_moment <- min(data[[datecol]])
  year_min <- lubridate::year(start_moment)
  month_min <- lubridate::month(start_moment)

  end_moment <- max(data[[datecol]])
  year_max <- lubridate::year(end_moment)
  month_max <- lubridate::month(end_moment)

  timetk::tk_ts(data,
                select = select,
                start = c(year_min, month_min),
                end = c(year_max, month_max),
                frequency = freq
                )

}
