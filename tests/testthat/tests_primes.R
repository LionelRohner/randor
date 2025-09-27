# Libs --------------------------------------------------------------------

library(testthat)
source("lib/primes.R", encoding = "UTF-8")

# Unit Tests --------------------------------------------------------------

testthat::test_that("Test that sieve_ov_Erathostenes works as intended", {
  # Dummy data
  x1 <- 10
  x2 <- 1e6
  x3 <- 2
  x4 <- 3
  x5 <- 1
  x6 <- -1

  # Expected results
  exp_x1 <- c(2, 3, 5, 7)
  exp_x2_len <- 78498
  exp_x3 <- x3
  exp_x4 <- x4

  # Tests
  actual_x1 <- sieve_ov_Erathostenes(to = x1)
  expect_equal(actual_x1, exp_x1)

  actual_x2_len <- sieve_ov_Erathostenes(to = x2) %>% length()
  expect_equal(actual_x2_len, exp_x2_len)

  actual_x3 <- sieve_ov_Erathostenes(to = x3)
  expect_equal(actual_x3, exp_x3)

  actual_x4 <- sieve_ov_Erathostenes(to = x4)
  expect_equal(actual_x4, exp_x4)

  expect_error(sieve_ov_Erathostenes(to = x5))
  expect_error(sieve_ov_Erathostenes(to = x6))
})

testthat::test_that("Test that get_circular_primes works as intended", {
  # Dummy data
  p1 <- sieve_ov_Erathostenes(to = 10)
  p2 <- sieve_ov_Erathostenes(to = 1000)
  p3 <- sieve_ov_Erathostenes(to = 1e6)
  p4 <- 6

  # Expected results
  exp_p1 <- c(2, 3, 5, 7)
  exp_p2 <- c(
    2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, 97, 113, 131, 197,
    199, 311, 337, 373, 719, 733, 919, 971, 991
  )
  exp_p3_len <- 55

  # Tests
  actual_p1 <- get_circular_primes(primes = p1)
  expect_equal(actual_p1, exp_p1)

  actual_p2 <- get_circular_primes(primes = p2)
  expect_equal(actual_p2, exp_p2)

  actual_p3_len <- get_circular_primes(primes = p3) %>% length()
  expect_equal(actual_p3_len, exp_p3_len)

  expect_null(get_circular_primes(primes = p4))
})

testthat::test_that("coin_change_algo gives correct number of combinations", {
  skip("coin_change_algo is not correctly implemented yet.")

  actual_1 <- coin_change_algo(c(1, 2, 5), 5)
  expected_1 <- 4
  expect_equal(actual_1, expected_1)

  actual_2 <- coin_change_algo(c(1, 2), 4)
  expected_2 <- 3
  expect_equal(actual_2, expected_2)
})
