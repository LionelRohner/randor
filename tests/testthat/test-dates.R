library(testthat)

context("dates")

test_that("calc_age calculates age correctly", {
  # Simple case
  expect_equal(calc_age("1990-01-01", "2020-01-01"), 30)

  # Check floor argument
  expect_equal(calc_age("1990-01-01", "2020-06-01"), 30)
  expect_gt(calc_age("1990-01-01", "2020-06-01", floor = FALSE), 30)

  # Edge case: birthday today
  today <- Sys.Date()
  birth_date <- as.Date(paste0(as.numeric(format(today, "%Y")) - 25, format(today, "-%m-%d")))
  expect_equal(calc_age(birth_date, today), 25)

  # Edge case: day before birthday
  yesterday <- today - 1
  expect_equal(calc_age(birth_date, yesterday), 24)
})
