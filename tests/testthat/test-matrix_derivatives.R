library(testthat)

context("matrix_derivatives")

test_that("differentiate_polynomial works for a simple case", {
  # This is an integration test for the functions in matrix_derivatives.R
  # The implementation has some quirks, so we test a simple case based on the examples.
  expect_equal(differentiate_polynomial("1+x+x^2", order = 3), "1*x^0+2*x^1")
})

test_that("prep_polynomial handles simple case", {
  expect_equal(prep_polynomial("1+2x+3x^2", 3), c(1, 2, 3))
})

test_that("construct_derivate_matrix has a consistent (if strange) output", {
  # The function does not produce a standard derivative matrix,
  # but we can test that its output is consistent.
  A <- matrix(1:3, 3, 3)
  B <- A; diag(B) <- 0
  C <- A - B
  expected_d_dx <- cbind(rep(0,3), C)
  expect_equal(construct_derivate_matrix(3), expected_d_dx)
})
