context("matrix_derivatives")

test_that("differentiate_polynomial works for cases from issue", {
  expect_equal(differentiate_polynomial("x^2", order = 2), "2*x^1")
  expect_equal(differentiate_polynomial("x+x^10", order = 10), "1*x^0+10*x^9")
  expect_equal(differentiate_polynomial("1-x-x^2", order = 2), "-1*x^0-2*x^1")
})
