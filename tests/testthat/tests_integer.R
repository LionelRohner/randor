# Libs --------------------------------------------------------------------

library(testthat)
source("lib/integers.R", encoding = "UTF-8")

# Unit Tests --------------------------------------------------------------

testthat::test_that("Test that get_digits works as intended", {
  # Dummy data
  x1 <- 1
  x2 <- 12
  x3 <- 123456789
  x4 <- -123
  x5 <- 6.1
  x6 <- 0
  x7 <- 8 # Test with base 2

  # Expected results
  exp_x1 <- 1
  exp_x2 <- 2
  exp_x3 <- 9
  # x4 should throw error
  exp_x5 <- 1
  exp_x6 <- -Inf
  exp_x7 <- 4 # 8 in base 2 is 1000

  # Tests
  actual_x1 <- get_digits(x = x1)
  expect_equal(actual_x1, exp_x1)

  actual_x2 <- get_digits(x = x2)
  expect_equal(actual_x2, exp_x2)

  actual_x3 <- get_digits(x = x3)
  expect_equal(actual_x3, exp_x3)

  expect_error(get_digits(x = x4))

  actual_x5 <- get_digits(x = x5)
  expect_equal(actual_x5, exp_x5)

  actual_x6 <- get_digits(x = x6)
  expect_equal(actual_x6, exp_x6)

  actual_x7 <- get_digits(x = x7, base = 2)
  expect_equal(actual_x7, exp_x7)
})

testthat::test_that("Test that get_powers_of_ten works as intended", {
  # Dummy data
  x1 <- 0
  x2 <- 1
  x3 <- 6
  x4 <- -1
  x5 <- 3
  x6 <- 7

  # Expected results
  # x1 should throw an error
  exp_x2 <- c(10)
  exp_x3 <- c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6)
  # x4 should throw an error
  exp_x5 <- c(1, 10, 100, 1000)
  exp_x6 <- 1e7

  # Tests
  expect_error(get_powers_of_ten(ndigits = x1))

  actual_x2 <- get_powers_of_ten(ndigits = x2)
  expect_equal(actual_x2, exp_x2)

  actual_x3 <- get_powers_of_ten(ndigits = x3)
  expect_equal(actual_x3, exp_x3)

  expect_error(get_powers_of_ten(ndigits = x4))

  actual_x5 <- get_powers_of_ten(ndigits = x5, include_one = TRUE)
  expect_equal(actual_x5, exp_x5)

  actual_x6 <- get_powers_of_ten(ndigits = x6, full_seq = FALSE)
  expect_equal(actual_x6, exp_x6)
})


testthat::test_that("Test that concatenate_math works as intended", {
  # Dummy data
  x1 <- 1
  y1 <- 2
  x2 <- 12
  y2 <- 345678
  x3 <- 10
  y3 <- 30
  x4 <- 0
  y4 <- 1
  x5 <- 1
  y5 <- 0

  # Expected
  exp_1 <- 12
  exp_2 <- 12345678
  exp_3 <- 1030
  exp_4 <- 1

  # Test
  actual_1 <- concatenate_math(x = x1, y = y1)
  expect_equal(actual_1, exp_1)

  actual_2 <- concatenate_math(x = x2, y = y2)
  expect_equal(actual_2, exp_2)

  actual_3 <- concatenate_math(x = x3, y = y3)
  expect_equal(actual_3, exp_3)

  actual_4 <- concatenate_math(x = x4, y = y4)
  expect_equal(actual_4, exp_4)

  expect_error(concatenate_math(x = x5, y = y5))
})

testthat::test_that("Test that transform_to_base_ten works as intended", {
  # Dummy data
  x1 <- 1001
  x2 <- 11
  x3 <- 123456789

  # Expected
  exp_1 <- 9
  exp_2 <- 9
  exp_3 <- 123456789

  # Test
  actual_1 <- transform_to_base_ten(x = x1, base = 2)
  expect_equal(actual_1, exp_1)

  actual_2 <- transform_to_base_ten(x = x2, base = 8)
  expect_equal(actual_2, exp_2)

  actual_3 <- transform_to_base_ten(x = x3, base = 10)
  expect_equal(actual_3, exp_3)
})

testthat::test_that("Test that extract_last_digits works as intended", {
  # Dummy data
  x1 <- 1
  x2 <- 12
  x3 <- 123
  x4 <- 1234

  # Expected
  exp_1 <- 1
  exp_2 <- 2
  exp_3 <- 3
  exp_4 <- 4

  # Test
  actual_1 <- extract_last_digits(x = x1)
  expect_equal(actual_1, exp_1)

  actual_2 <- extract_last_digits(x = x2)
  expect_equal(actual_2, exp_2)

  actual_3 <- extract_last_digits(x = x3)
  expect_equal(actual_3, exp_3)

  actual_4 <- extract_last_digits(x = x4)
  expect_equal(actual_4, exp_4)
})

testthat::test_that("Test that extract_digits works as intended", {
  # Dummy data
  x1 <- 1
  x2 <- 123456789
  x3 <- c(1, 22, 333)
  x4 <- c(10, 100, 101013)
  x5 <- 0

  # Expected
  exp_1 <- c(1)
  exp_2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% sort()
  exp_3 <- c(1, 2, 2, 3, 3, 3) %>% sort()
  exp_4 <- c(1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 3) %>% sort()

  # Test
  actual_1 <- extract_digits(x = x1)
  expect_equal(actual_1, exp_1)

  actual_2 <- extract_digits(x = x2) %>% sort()
  expect_equal(actual_2, exp_2)

  actual_3 <- extract_digits(x = x3) %>% sort()
  expect_equal(actual_3, exp_3)

  actual_4 <- extract_digits(x = x4) %>% sort()
  expect_equal(actual_4, exp_4)

  expect_error(extract_digits(x = x5))
})


testthat::test_that("Test that extract_digits_matrix works as intended", {
  # Dummy data
  x1 <- 1
  x2 <- 123456789
  x3 <- c(1, 22, 333)
  x4 <- c(10, 100, 101013)
  x5 <- 0

  # Expected
  exp_1 <- c(1)
  exp_2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% sort()
  exp_3 <- c(1, 2, 2, 3, 3, 3) %>% sort()
  exp_4 <- c(1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 3) %>% sort()

  # Test
  actual_1 <- extract_digits_matrix(x = x1)
  expect_equal(actual_1, exp_1)

  actual_2 <- extract_digits_matrix(x = x2) %>% sort()
  expect_equal(actual_2, exp_2)

  actual_3 <- extract_digits_matrix(x = x3) %>% sort()
  expect_equal(actual_3, exp_3)

  actual_4 <- extract_digits_matrix(x = x4) %>% sort()
  expect_equal(actual_4, exp_4)

  expect_error(extract_digits_matrix(x = x5))
})

testthat::test_that("is_divisible_by_digit is not implemented", {
  skip("is_divisible_by_digit is not implemented yet.")
})

context("plot")

test_that("plot_all_digits runs without error", {
  vec <- c(1, 22, 333)
  # This will fail if the function throws an error
  expect_silent(plot_all_digits(vec))
})

test_that("plot_last_digits runs without error", {
  vec <- c(1, 22, 333)
  # This will fail if the function throws an error
  expect_silent(plot_last_digits(vec))
})
