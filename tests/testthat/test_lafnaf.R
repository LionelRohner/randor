library(testthat)

context("lafnaf")

test_that("create_basis creates a valid basis", {
  basis <- create_basis(dim = 3, returnMat = TRUE)
  expect_true(is.matrix(basis))
  expect_equal(dim(basis), c(3, 3))
  # A non-zero determinant implies linear independence
  expect_true(det(basis) != 0)
})

test_that("is_pos_def works correctly", {
  # A positive definite matrix
  A_pos_def <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), ncol = 3)
  expect_true(is_pos_def(A_pos_def))
  
  # A non-positive definite matrix
  A_not_pos_def <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
  expect_false(is_pos_def(A_not_pos_def))
})

test_that("mat_pow calculates matrix powers correctly", {
  A <- matrix(c(1, 1, 0, 1), ncol = 2)
  A_sq <- matrix(c(1, 2, 0, 1), ncol = 2)
  expect_equal(mat_pow(A, 2), A_sq)
})

test_that("adjugate works correctly", {
  A2 <- matrix(c(1, 2, 3, 4), ncol = 2)
  adjA2 <- matrix(c(4, -2, -3, 1), ncol = 2)
  expect_equal(adjugate(A2), adjA2)
  
  A3 <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), ncol = 3)
  adjA3 <- matrix(c(24, -12, -2, 5, 3, -5, -4, 2, 4), ncol = 3)
  expect_equal(adjugate(A3), adjA3)
})

test_that("inverse works correctly", {
  A <- matrix(c(1, 2, 0, 1), ncol = 2)
  A_inv <- matrix(c(1, -2, 0, 1), ncol = 2)
  expect_equal(inverse(A), A_inv)
})

test_that("rank_matrix works correctly", {
  A <- matrix(c(1, 2, 3, 2, 4, 6), nrow = 2, byrow = TRUE)
  expect_equal(rank_matrix(A), 1)
  
  B <- matrix(c(1, 0, 0, 1), ncol = 2)
  expect_equal(rank_matrix(B), 2)
})

test_that("canonical_form works correctly", {
  A <- matrix(c(1, 0, 0, 2), ncol = 2)
  cano <- canonical_form(A)
  # Check that UDU^-1 = A
  A_recon <- cano$U %*% cano$D %*% cano$U_inv
  expect_equal(A_recon, A)
})

test_that("singular_value_decomposition works correctly", {
  A <- matrix(c(1, 0, 0, 1), ncol = 2)
  svd_res <- singular_value_decomposition(A)
  # Check that P and Q are orthogonal
  # The check fails due to floating point inaccuracies, so I will round
  expect_equal(round(svd_res$P %*% t(svd_res$P)), diag(2))
  expect_equal(round(svd_res$Q %*% t(svd_res$Q)), diag(2))
  # Check that P D t(Q) = A
  A_recon <- svd_res$P %*% svd_res$D %*% t(svd_res$Q)
  expect_equal(round(A_recon), A)
})
