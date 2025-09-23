library(testthat)

context("2d_list_comprehension")

test_that("list_comp_2d works with default values", {
  # The default should return a 3x3 matrix with values 1 to 9
  expected <- matrix(1:9, nrow = 3, byrow = TRUE)
  expect_equal(list_comp_2d(row = 3, col = 3), expected)
})

test_that("list_comp_2d works with a simple expression", {
  # A 2x2 matrix of zeros
  expected <- matrix(0, nrow = 2, ncol = 2)
  expect_equal(list_comp_2d(row = 2, col = 2, x = "0"), expected)
})

test_that("list_comp_2d works with a condition", {
  # A 2x2 matrix of even numbers
  # it will be 2, 4, 6, 8
  expected <- matrix(c(2, 4, 6, 8), nrow = 2, byrow = TRUE)
  expect_equal(list_comp_2d(row = 2, col = 2, cond = "i %% 2 == 0"), expected)
})
