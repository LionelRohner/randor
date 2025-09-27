library(testthat)

context("estimate_pi")

test_that("generate_points creates a matrix of correct dimensions", {
  n <- 100
  points <- generate_points(n)
  expect_true(is.matrix(points))

  actual_rows <- nrow(points)
  expected_rows <- n
  expect_equal(actual_rows, expected_rows)

  actual_cols <- ncol(points)
  expected_cols <- 2
  expect_equal(actual_cols, expected_cols)
})

test_that("get_distance works correctly", {
  # Point inside the unit circle
  p_in <- matrix(c(0.5, 0.5), nrow = 1)
  expect_true(get_distance(p_in))

  # Point outside the unit circle
  p_out <- matrix(c(1, 1), nrow = 1)
  expect_false(get_distance(p_out))
})

test_that("approx_pi works correctly", {
  # If all points are inside the circle, the ratio is 1, so pi should be 4
  dist_all_in <- c(TRUE, TRUE, TRUE, TRUE)
  actual_all_in <- approx_pi(dist_all_in)
  expected_all_in <- 4
  expect_equal(actual_all_in, expected_all_in)

  # If half are inside, ratio is 0.5, pi should be 2
  dist_half_in <- c(TRUE, TRUE, FALSE, FALSE)
  actual_half_in <- approx_pi(dist_half_in)
  expected_half_in <- 2
  expect_equal(actual_half_in, expected_half_in)
})

test_that("estimate_pi_empirical returns a reasonable value", {
  pi_est <- estimate_pi_empirical(10000)
  expect_true(is.numeric(pi_est))

  actual_length <- length(pi_est)
  expected_length <- 1
  expect_equal(actual_length, expected_length)

  # A very loose check for plausibility
  expect_true(pi_est > 2.5 && pi_est < 4.5)
})

test_that("beta_mom calculates moments correctly", {
  x <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  mom <- beta_mom(x)
  expect_true(is.list(mom))

  actual_length <- length(mom)
  expected_length <- 2
  expect_equal(actual_length, expected_length)

  expect_named(mom, c("shape1", "shape2"))

  # Values calculated manually
  actual_shape1 <- round(mom$shape1, 2)
  expected_shape1 <- 1.25
  expect_equal(actual_shape1, expected_shape1)

  actual_shape2 <- round(mom$shape2, 2)
  expected_shape2 <- 1.75
  expect_equal(actual_shape2, expected_shape2)
})

test_that("calc_ratio returns a vector of correct length", {
  n <- 10
  samplingSize <- 5
  ratios <- calc_ratio(n, samplingSize, plot = FALSE)
  expect_true(is.vector(ratios))

  actual_length <- length(ratios)
  expected_length <- samplingSize
  expect_equal(actual_length, expected_length)
})

test_that("approx_pi_resample works correctly", {
  # If the mean of the random vector is 0.785 (approx pi/4), then the result should be pi
  randVec <- rep(pi / 4, 10)
  actual <- approx_pi_resample(randVec)
  expected <- pi
  expect_equal(actual, expected)
})
