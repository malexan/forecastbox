library(forecast)
data("gas", package = "forecast")
gas24 <- subset(gas, end = 24L)
ets_frcst <- function(x, h) forecast(ets(x), h = h)

context("TS cross validation")

test_that("crossv_ts returns a list", {
  expect_is(crossv_ts(gas24, ets, tail2CV = 2),
               "list")
})

test_that("crossv_ts returns a list of proper length", {
  expect_length(crossv_ts(gas24, ets, h = 1, tail2CV = 2),
               2)
})
