#' Selects model with minimal mean absolute forecating error.
#'
#' @param ... Arguments passed to \code{crossv_ts_multi}.
#' @return Name of model function which produced smallest error.
#' @export

best_mdl <- function(...) {

  err_ts_lst <- crossv_ts_multi(...)

  mean_errs <- purrr::map_dbl(err_ts_lst, ~ mean(abs(.), na.rm = TRUE))

  mdl <- names(mean_errs[mean_errs == min(mean_errs)])

  if (length(mdl) == 1L) warning("Result of model selection is not length one")

  mdl

}

