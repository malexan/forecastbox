#' Makes predictions
#'
#' @import dplyr
#' @import purrr

run_predict <- function(tbl, samplesize = NULL) {

  if (!is.null(samplesize)) {

    stopifnot(is.numeric(samplesize))

    if (nrow(tbl) > samplesize) {

      tbl <- tbl %>%
        filter(tsid %in% sample(unique(tbl$tsid), samplesize))

    } else message("Sampling will not be used.")
  }

  # Convert to ts
  by_id <- tbl %>%
    group_by(tsid) %>%
    arrange(month) %>%
    tidyr::nest() %>%
    mutate(ts = map(data, convert_df2ts, "m3day"))

  by_id <- by_id %>%
    mutate(fit = map(ts, forecast::bats)) %>%
    mutate(pred = map(fit, ~ forecast::forecast(.x, h = 1))) %>%
    mutate(predtbl = map(pred, function(x) {
      x %>%
        sweep::sw_sweep(rename_index = "month") %>%
        filter(key == "forecast") %>%
        select(-key) %>%
        mutate(month = zoo::as.Date.yearmon(month))
    }
    ))

  by_id %>%
    tidyr::unnest_("predtbl", .drop = TRUE) %>%
    mutate_if(is.numeric, funs(if_else(. <= 0, 0, .)))

}
