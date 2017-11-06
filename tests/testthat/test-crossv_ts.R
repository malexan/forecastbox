library(forecast)
data("gas", package = "forecast")
gas24 <- subset(gas, end = 24L)
gas6 <- subset(gas, end = 6L)
ets_frcst <- function(x, h) forecast(ets(x), h = h)
bats_frcst <- function(x, h) forecast(bats(x), h = h)

context("TS cross validation")

test_that("crossv_ts returns a ts", {
  expect_is(crossv_ts(gas24, ets_frcst, tail2CV = 2),
               "ts")
})

test_that("crossv_ts returns a ts of proper length", {
  expect_length(crossv_ts(gas24, ets_frcst, h = 1, tail2CV = 2),
               2)
  expect_length(crossv_ts(gas24, ets_frcst, h = 2, tail2CV = 2),
                1)
})

# test_that("crossv_ts returns a ts with proper number of NA values", {
#   expect_equal(sum(is.na(crossv_ts(gas6, ets_frcst, h = 1, tail2CV = 2))),
#                4)
#   expect_equal(sum(is.na(crossv_ts(gas6, ets_frcst, h = 2, tail2CV = 2))),
#                5)
# })
#
