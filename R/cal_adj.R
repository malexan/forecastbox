#' Adjusts work days.
#'
#' @import assertthat
#' @import dplyr

cal_adj <- function(tbl) {

 tbl <- tbl %>%
    left_join(workdays, by = "month")

 assert_that(noNA(tbl$count))

 tbl <- tbl %>%
   mutate(m3day = m3 / count) %>%
   select(-count)

 assert_that(noNA(tbl$m3day))

 tbl

}

#' Adjusts work days back.
#'
#' @import assertthat
#' @import dplyr

cal_adj_back <- function(tbl) {

 tbl <- tbl %>%
    left_join(workdays, by = "month")

 assert_that(noNA(tbl$count))

 tbl <- tbl %>%
   mutate_if(is.numeric, funs(. * count)) %>%
   select(-count) %>%
   rename(m3 = m3day) %>%
   mutate(month = format(month, "%Y-%m"))

 tbl

}
