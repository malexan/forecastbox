#' Imports an Excel file with time series data.
#'
#' @import assertthat
#' @import purrr
#' @param filepath Path to excel file

import_excel <- function(filepath = NULL) {

  stopifnot(file.exists(filepath))

  tbl <- readxl::read_excel(filepath)

  # First column contains clients' ids
  # No duplicates are expected
  assert_that(sum(duplicated(tbl[[1]])) == 0)

  walk(tbl[-1], ~ assert_that(is.numeric(.x)))

  tbl

}
