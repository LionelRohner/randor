library(testthat)

context("estimate_pi")

test_that("generate_points creates a matrix of correct dimensions", {
  n <- 100
  points <- generate_points(n)
  expect_true(is.matrix(points))
  expect_equal(nrow(points), n)
  expect_equal(ncol(points), 2)
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
  dist <- c(TRUE, TRUE, TRUE, TRUE)
  expect_equal(approx_pi(dist), 4)

  # If half are inside, ratio is 0.5, pi should be 2
  dist <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(approx_pi(dist), 2)
})

test_that("estimate_pi_empirical returns a reasonable value", {
  pi_est <- estimate_pi_empirical(10000)
  expect_true(is.numeric(pi_est))
  expect_equal(length(pi_est), 1)
  # A very loose check for plausibility
  expect_true(pi_est > 2.5 && pi_est < 4.5)
})

test_that("beta_mom calculates moments correctly", {
  x <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  mom <- beta_mom(x)
  expect_true(is.list(mom))
  expect_equal(length(mom), 2)
  expect_named(mom, c("shape1", "shape2"))
  # Values calculated manually
  expect_equal(round(mom$shape1, 2), 1.25)
  expect_equal(round(mom$shape2, 2), 1.75)
})

test_that("calc_ratio returns a vector of correct length", {
  n <- 10
  samplingSize <- 5
  ratios <- calc_ratio(n, samplingSize, plot = FALSE)
  expect_true(is.vector(ratios))
  expect_equal(length(ratios), samplingSize)
})

test_that("approx_pi_resample works correctly", {
  # If the mean of the random vector is 0.785 (approx pi/4), then the result should be pi
  randVec <- rep(pi/4, 10)
  expect_equal(approx_pi_resample(randVec), pi)
})
