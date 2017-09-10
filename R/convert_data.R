#' Converts raw data to tidy format
#'
#' @import assertthat
#' @import dplyr

convert_data <- function(tbl) {

  oldscipen <- getOption("scipen")
  on.exit(options(scipen = oldscipen))
  options(scipen = 999L)

  colnames(tbl)[1] <- "tsid"

  maxidlen <- max(stringr::str_length(tbl$tsid))

  tbl <- tbl %>%
  tidyr::gather(month, m3, -tsid) %>%
    mutate(tsid = stringr::str_pad(tsid, width = maxidlen,
                          pad = "0"),
           month = lubridate::ymd(paste0(month, "-01")))

  assert_that(noNA(tbl$month))

  tbl

}
