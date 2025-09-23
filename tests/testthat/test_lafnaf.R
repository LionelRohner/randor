library(testthat)

context("lafnaf")

test_that("create_basis creates a valid basis", {
  basis <- create_basis(dim = 3, returnMat = TRUE)
  expect_true(is.matrix(basis))

  actual_dim <- dim(basis)
  expected_dim <- c(3, 3)
  expect_equal(actual_dim, expected_dim)

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
  actual <- mat_pow(A, 2)
  expected <- matrix(c(1, 2, 0, 1), ncol = 2)
  expect_equal(actual, expected)
})

test_that("adjugate works correctly", {
  A2 <- matrix(c(1, 2, 3, 4), ncol = 2)
  actual_adjA2 <- adjugate(A2)
  expected_adjA2 <- matrix(c(4, -2, -3, 1), ncol = 2)
  expect_equal(actual_adjA2, expected_adjA2)
  
  A3 <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), ncol = 3)
  actual_adjA3 <- adjugate(A3)
  expected_adjA3 <- matrix(c(24, -12, -2, 5, 3, -5, -4, 2, 4), ncol = 3)
  expect_equal(actual_adjA3, expected_adjA3)
})

test_that("inverse works correctly", {
  A <- matrix(c(1, 2, 0, 1), ncol = 2)
  actual <- inverse(A)
  expected <- matrix(c(1, -2, 0, 1), ncol = 2)
  expect_equal(actual, expected)
})

test_that("rank_matrix works correctly", {
  A <- matrix(c(1, 2, 3, 2, 4, 6), nrow = 2, byrow = TRUE)
  actual_A <- rank_matrix(A)
  expected_A <- 1
  expect_equal(actual_A, expected_A)

  B <- matrix(c(1, 0, 0, 1), ncol = 2)
  actual_B <- rank_matrix(B)
  expected_B <- 2
  expect_equal(actual_B, expected_B)
})

test_that("canonical_form works correctly", {
  A <- matrix(c(1, 0, 0, 2), ncol = 2)
  cano <- canonical_form(A)
  # Check that UDU^-1 = A
  actual <- cano$U %*% cano$D %*% cano$U_inv
  expected <- A
  expect_equal(actual, expected)
})

test_that("singular_value_decomposition works correctly", {
  A <- matrix(c(1, 0, 0, 1), ncol = 2)
  svd_res <- singular_value_decomposition(A)

  # Check that P and Q are orthogonal
  actual_P_ortho <- round(svd_res$P %*% t(svd_res$P))
  expected_P_ortho <- diag(2)
  expect_equal(actual_P_ortho, expected_P_ortho)

  actual_Q_ortho <- round(svd_res$Q %*% t(svd_res$Q))
  expected_Q_ortho <- diag(2)
  expect_equal(actual_Q_ortho, expected_Q_ortho)

  # Check that P D t(Q) = A
  actual_recon <- round(svd_res$P %*% svd_res$D %*% t(svd_res$Q))
  expected_recon <- A
  expect_equal(actual_recon, expected_recon)
})
