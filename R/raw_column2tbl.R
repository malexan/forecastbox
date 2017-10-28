#' Converts text column with numbers to a tibble
#'
#' @importFrom lubridate "%m+%"
#' @export

raw_column2tbl <- function(raw_column = NULL,
                           dec = NULL,
                           startdate = NULL) {

  rawdata <- read.table(textConnection(raw_column),
                        header = FALSE,
                        dec = dec)
  rawdata <- rawdata[[1]]
  timevec <- startdate %m+%
    months(seq_along(rawdata) - 1)

  tibble::tibble(date = timevec,
                 value = rawdata)

}
