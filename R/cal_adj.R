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
