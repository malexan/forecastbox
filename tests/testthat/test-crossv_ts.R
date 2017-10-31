library(forecast)
data("gas", package = "forecast")
gas24 <- subset(gas, end = 24L)

context("TS cross validation")

test_that("crossv_ts returns a list", {
  expect_is(crossv_ts(gas24, ets, tail2CV = 2),
               "list")
})
