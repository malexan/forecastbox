#' Makes predictions
#'
#' @import dplyr
#' @import purrr

run_predict <- function(tbl) {

  # Convert to ts
  by_id <- tbl %>%
    group_by(tsid) %>%
    arrange(month) %>%
    tidyr::nest() %>%
    mutate(ts = map(data, convert_df2ts, "m3day"))

  by_id <- by_id %>%
    mutate(fit = map(ts, forecast::bats)) %>%
    mutate(pred = map(fit, ~ forecast::forecast(.x, h = 1))) %>%
    mutate(predtbl = map_dfr(pred, sweep::sw_sweep))

  by_id

}
