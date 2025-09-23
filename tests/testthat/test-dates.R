library(testthat)

context("dates")

test_that("calc_age calculates age correctly", {
  # Simple case
  actual_simple <- calc_age("1990-01-01", "2020-01-01")
  expected_simple <- 30
  expect_equal(actual_simple, expected_simple)

  # Check floor argument
  actual_floor <- calc_age("1990-01-01", "2020-06-01")
  expected_floor <- 30
  expect_equal(actual_floor, expected_floor)

  actual_no_floor <- calc_age("1990-01-01", "2020-06-01", floor = FALSE)
  expected_no_floor_gt <- 30
  expect_gt(actual_no_floor, expected_no_floor_gt)

  # Edge case: birthday today
  today <- Sys.Date()
  birth_date <- as.Date(paste0(as.numeric(format(today, "%Y")) - 25, format(today, "-%m-%d")))
  actual_today <- calc_age(birth_date, today)
  expected_today <- 25
  expect_equal(actual_today, expected_today)

  # Edge case: day before birthday
  yesterday <- today - 1
  actual_yesterday <- calc_age(birth_date, yesterday)
  expected_yesterday <- 24
  expect_equal(actual_yesterday, expected_yesterday)
})
