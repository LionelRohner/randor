#' @importFrom graphics par grid arrows points legend text abline
#' @importFrom grDevices rgb
NULL

# Linear Algebra Functions Nobody Asked For -------------------------------


# None of the functions below have been thourougly tested!


# Main Functions ----------------------------------------------------------

#' Create a Basis of Linearly Independent Vectors
#'
#' This function generates a set of linearly independent vectors that form a basis for a vector space of a given dimension.
#'
#' @param dim An integer specifying the dimension of the basis.
#' @param negative A logical value indicating whether to include negative values in the random numbers used to generate the basis vectors. Defaults to `TRUE`.
#' @param upper An integer specifying the upper bound for the random numbers. Defaults to `9`.
#' @param returnMat A logical value indicating whether to return the basis as a matrix or a list of vectors. Defaults to `FALSE`.
#'
#' @return If `returnMat` is `TRUE`, a matrix where each column is a basis vector. If `FALSE`, a list of numeric vectors, where each vector is a basis vector.
#' @export
#'
#' @examples
#' # Create a 3-dimensional basis and return it as a list of vectors
#' create_basis(3)
#'
#' # Create a 2-dimensional basis with only positive values and return it as a matrix
#' create_basis(2, negative = FALSE, returnMat = TRUE)
create_basis <- function(dim, negative = TRUE, upper = 9, returnMat = FALSE) {
  if (negative) {
    lower <- -upper
  } else {
    lower <- 0
  }

  basis <- matrix(sample(lower:upper, dim^2, replace = TRUE), nrow = dim)

  while (det(basis) == 0) {
    basis <- matrix(sample(lower:upper, dim^2, replace = TRUE), nrow = dim)
  }

  if (returnMat) {
    return(basis) # return in matrix form
  }

  # output list of vecs instead of matrix
  out <- list()
  for (rowcol in 1:dim) {
    out[[rowcol]] <- basis[, rowcol]
  }
  return(out)
}

#' Check if a Matrix is Positive Definite
#'
#' This function checks if a given square matrix is positive definite. A matrix is positive definite if it is symmetric and all its eigenvalues are positive.
#'
#' @param A A numeric matrix.
#'
#' @return A logical value indicating whether the matrix is positive definite (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#' # A positive definite matrix
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' is_pos_def(A)
#'
#' # A non-positive definite matrix
#' B <- matrix(c(1, 2, 2, 1), nrow = 2)
#' is_pos_def(B)
is_pos_def <- function(A) { # test 1 - symmetry

  # test 1 - is symmetric?
  test1 <- all(round(A, 5) == round(t(A), 5))

  message("Test 1 - Matrix is symmetric? ", test1)

  # test 2 - positive eigenvalues
  test2 <- prod(Re(eigen(A)$value)) > 0

  message("Test 2 - All (real) eigenvalues are positive? ", test2)

  # test 3 - positive upper left submatrices
  dimA <- nrow(A)
  upLeftDets <- c(A[1, 1])
  if (A[1, 1] < 0) {
    test3 <- FALSE
  } else {
    for (i in 2:dimA) {
      upLeftDet <- det(A[1:i, 1:i])
      if (upLeftDet < 0) {
        test3 <- FALSE
      } else {
        upLeftDets <- c(upLeftDets, det(A[1:i, 1:i]))
      }
    }
  }
  test3 <- all(upLeftDets > 0)

  message("Test 3 - Determinant of upper left submatrices are > 0? ", test3)

  # output
  return(all(test1, test2, test3))
}

#' Create Canonical Form (Diagonalization) of a Matrix
#'
#' This function computes the canonical form (or diagonalization) of a square matrix `A`. The canonical form is represented by the equation `A = UDU^-1`, where `U` is the matrix of eigenvectors, `D` is the diagonal matrix of eigenvalues, and `U_inv` is the inverse of `U`.
#'
#' @param A A numeric matrix.
#'
#' @return A list containing the matrices `U`, `D`, and `U_inv`.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' canonical_form(A)
canonical_form <- function(A) {
  if (is_defective(A)) {
    stop("Matrix is defective and cannot be diagonalized.")
  }
  # create UDU

  # create U
  eign <- eigen(A)
  U <- eign$vectors

  # create D
  D <- matrix(0, nrow = length(eign$values), ncol = length(eign$values))
  diag(D) <- eign$values

  # create U-1
  U_inv <- adjugate(U) * 1 / det(U)

  # output
  res <- list(U = U, D = D, U_inv = U_inv)
  return(res)
}

#' Check if a Matrix is Defective
#'
#' This function checks if a given square matrix is defective. A matrix is defective if the geometric multiplicity of any of its eigenvalues is less than the algebraic multiplicity.
#'
#' @param A A numeric matrix.
#'
#' @return A logical value indicating whether the matrix is defective (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#' # A defective matrix
#' A <- matrix(c(1, 1, 0, 1), nrow = 2)
#' is_defective(A)
#'
#' # A non-defective matrix
#' B <- matrix(c(1, 0, 0, 2), nrow = 2)
#' is_defective(B)
is_defective <- function(A) {
  e <- eigen(A)
  # Check if the number of linearly independent eigenvectors is less than n
  return(qr(e$vectors)$rank < nrow(A))
}

#' Fast Matrix Exponentiation using Canonical Form
#'
#' This function calculates the power of a square matrix using its canonical form (diagonalization). This method is generally more efficient for large powers than repeated matrix multiplication.
#'
#' @param A A numeric matrix.
#' @param p An integer specifying the power to raise the matrix to.
#'
#' @return The matrix `A` raised to the power of `p`.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' fast_exp(A, 2)
fast_exp <- function(A, p) {
  UDU <- canonical_form(A)

  # fast exponentiation of eigenvalues
  D <- UDU$D
  D_to_the_p <- diag(D)^p

  # calculate UD^pU^-1
  # U doesnt have to be included in the power calc, because A = UDU^-1, thus A^2 = UDU^-1UDU^-1, which is UDDU^-1
  diag(D) <- D_to_the_p

  UDU_inv <- UDU$U %*% D %*% UDU$U_inv

  return(UDU_inv)
}

#' Calculate Matrix Powers
#'
#' This function calculates the power of a square matrix by repeated matrix multiplication.
#'
#' @param A A numeric matrix.
#' @param n An integer specifying the power to raise the matrix to.
#'
#' @return The matrix `A` raised to the power of `n`.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' mat_pow(A, 2)
mat_pow <- function(A, n) {
  # TODO: Cheap workaround...
  n <- n - 1

  if (n == 0) {
    return(A)
  } else if (n == -1) {
    # return identity matrix
    I <- matrix(0, nrow = nrow(A), ncol = ncol(A))
    diag(I) <- rep(1, nrow(A))
    return(I)
  }

  A_to_the_i <- A
  for (i in 1:n) {
    A_to_the_i <- A %*% A_to_the_i
  }
  return(A_to_the_i)
}


#  ---------------------------------------

#' Cauchy-Schwarz Inequality Test for Linear Dependence
#'
#' This function uses the Cauchy-Schwarz inequality to test if two vectors are linearly dependent.
#'
#' @param u A numeric vector.
#' @param v A numeric vector.
#' @param tol A numeric value specifying the tolerance for the comparison. Defaults to `1e-05`.
#'
#' @return A logical value indicating whether the vectors are linearly dependent (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#' u <- c(1, 2)
#' v <- c(2, 4)
#' lin_dep_Cautchy_Schwartz(u, v)
lin_dep_Cautchy_Schwartz <- function(u, v, tol = 1e-05) {
  # Consists of checking whether <u,v> >= ||u|| ||v||
  # Strict equality indicate linear dependence, i.e. <u,v> = ||u|| ||v||

  # compute inner prod u^Tu and v^Tv
  uu <- u %*% u
  vv <- v %*% v

  # compute inner prod u^Tv
  uv_squared <- (u %*% v)^2

  # check for strict equality to find lin. dependent rows/cols
  if (compare_floats(uu * vv, uv_squared, tol = tol)) {
    message("Vectors are linearly dependent!")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Cauchy-Schwarz Inequality Test for Linear Dependence in a Matrix
#'
#' This function uses the Cauchy-Schwarz inequality to identify linearly dependent rows or columns in a matrix.
#'
#' @param A A numeric matrix.
#'
#' @return A data frame with the indices of the linearly dependent rows or columns, or `NULL` if none are found.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' lin_dep_Cautchy_Schwartz_matrix(A)
lin_dep_Cautchy_Schwartz_matrix <- function(A) {
  # Consists of checking whether <u,v> >= ||u|| ||v||
  # Strict equality indicate linear dependence, i.e. <u,v> = ||u|| ||v||

  # TODO: What about the commented out block below?
  # # Check whether there are more rows or cols, then choose shorter space
  # # This reduces the # of iterations in the double loop
  # if(ncol(A) > nrow(A)){
  #   A = t(A)
  # }

  # output container
  linDepIdx <- c()

  # Cauchy-Schwarzt Loop
  for (i in seq_len(nrow(A))) {
    # skip if the vector is a zero vector
    if (all(A[i, ] == 0)) {
      next
    }

    for (j in i:nrow(A)) {
      # skip if the vector is a zero vector
      if (all(A[j, ] == 0)) {
        next
      }

      if (i != j) {
        # compute norms and dot prod of the span of the row/col space
        u_norm <- sqrt(sum(A[i, ]^2))
        v_norm <- sqrt(sum(A[j, ]^2))
        u_v <- A[i, ] %*% A[j, ]

        # check for strict equality to find lin. dependent rows/cols
        if (compare_floats(u_v, u_norm * v_norm)) {
          # mirrored duplicates should be excluded
          linDepIdx <- rbind.data.frame(linDepIdx, c(i, j))
        }
      }
    }
  }

  # output construction
  if (is.null(nrow(linDepIdx))) {
    # message("No linear dependent cols/rows were found!")
    return(NULL)
  } else {
    colnames(linDepIdx) <- c("i", "j")
    # message("Linear combinations of cols/rows were found!")
    return(linDepIdx)
  }
}

#' Create an Adjugate Matrix
#'
#' This function calculates the adjugate matrix of a square matrix. The adjugate matrix is the transpose of the cofactor matrix.
#'
#' @param A A numeric matrix.
#'
#' @return The adjugate of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' adjugate(A)
adjugate <- function(A) {
  n <- nrow(A)
  m <- ncol(A)

  if (n != m) {
    message("Matrix must be square!")
    return(NULL)
  }

  # create emtpy cofactor matrix
  C <- matrix(NA, nrow = n, ncol = m)

  # special case for A of order 2x2
  if (n == 2) {
    C <- (-1) * A
    diag(C) <- rev(diag(A))
    return(C)
  }

  # populate the cofactor matrix
  for (i in 1:n) {
    for (j in 1:m) {
      C[i, j] <- (-1)^(i + j) * det(A[-i, -j])
    }
  }

  return(t(C))
}


# TODO: FIX LIMITATIONS
# does only work for obvious cases of lin. dep. (i.e. parallel vectors) that are
# identified by the Cautchy-Schwartz Inequality (e.g. [2,4] = 2*[1,2]), but not
# if lin. dep. arise from combinations of vectors (e.g. [4,0] = [2,-2] + [2,2]).

#' Create a Generalized Inverse of a Matrix
#'
#' This function calculates the generalized inverse (or Moore-Penrose inverse) of a matrix.
#'
#' @param A A numeric matrix.
#'
#' @return The generalized inverse of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' generalized_inverse(A)
generalized_inverse <- function(A) {
  ### step 1 : Find a LIN submatrix of order rxr
  message("Step 1 : Find a LIN submatrix W of order rxr in A!")

  nc <- ncol(A)
  nr <- nrow(A)

  rank <- rank_matrix(A)

  # Check whether A is full rank anyways
  if (nc == nr && det(A) != 0) {
    W <- A
  } else if (rank == 1) {
    idxRow <- idxCol <- 1
    W <- A[idxRow, idxCol, drop = FALSE]
  } else {
    # make matrix square
    outerBreak <- FALSE
    for (i in 1:(nr - rank + 1)) {
      for (j in 1:(nc - rank + 1)) {
        print(i:(i + rank - 1))
        print(j:(j + rank - 1))
        print(A[i:(i + rank - 1), j:(j + rank - 1)])
        print(det(A[i:(i + rank - 1), j:(j + rank - 1)]))
        if (det(A[i:(i + rank - 1), j:(j + rank - 1)]) != 0) {
          idxRow <- c(i:(i + rank - 1))
          idxCol <- c(j:(j + rank - 1))
          outerBreak <- TRUE
          break
        }
        if (outerBreak) {
          break
        }
      }
    }

    if (!outerBreak) {
      message("All submatrices are singular! Generalized Inverse could not be calculated!")
      return(NULL)
    }

    W <- A[idxRow, idxCol]
  }


  ### step 2 : (W^-1)^T
  message("Step 2 : (W^-1)^T!")

  # create adjoint matrix - adjoint() doesnt work for 2x2
  if (dim(W)[1] == 2) {
    adj_W <- -1 * W
    diag(adj_W) <- rev(diag(W))
  } else {
    adj_W <- adjugate(W)
  }

  transp_inv_W <- t(adj_W / det(W))

  ### step 3 : replace elements of A with (W^-1)^T
  message("Step 3 : Project rows/cols of (W^-1)^T into a a zero matrix A0 of order dim(A)!")

  # Again, if A is non-singular from scratch skip Step 3
  if (nc == nr && det(A) != 0) {
    A0 <- transp_inv_W
  } else {
    A0 <- 0 * A
    A0[idxRow, idxCol] <- transp_inv_W
  }


  ### step 4 : t(A)
  message("Step 3 : G = A0^T!")

  G <- t(A0)

  return(G)
}

#' Check Penrose Conditions of a Generalized Inverse
#'
#' This function checks if a given matrix `G` is a generalized inverse of a matrix `A` by verifying the Penrose conditions.
#'
#' @param A A numeric matrix.
#' @param G The generalized inverse of `A`.
#' @param all_Penrose_check A logical value indicating whether to check all four Penrose conditions. If `FALSE` (the default), only the first condition (`AGA = A`) is checked.
#' @param digits An integer specifying the number of digits to round to when comparing the matrices. Defaults to `2`.
#'
#' @return A logical value indicating whether the Penrose conditions are met (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' G <- generalized_inverse(A)
#' check_Penrose_cond(A, G)
check_Penrose_cond <- function(A,
                               G,
                               all_Penrose_check = FALSE,
                               digits = 2) {
  message("Disclaimer: All matrices are transformed to pure integer matrices first, so consider that...\n")

  if (all_Penrose_check == FALSE) {
    Penrose_1 <- all(round(A %*% G %*% A, 2) == A)
    message("AGA=A is ", Penrose_1)
    return(Penrose_1)
  } else {
    Penrose_1 <- all(round(A %*% G %*% A) == A)
    message("AGA = A is ", Penrose_1)

    Penrose_2 <- all(round(G %*% A %*% G, digits = digits) == round(G, digits = digits))
    message("GAG = G is ", Penrose_2)

    Penrose_3 <- all(round(A %*% G) == t(round(A %*% G)))
    message("AG = (AG)' is ", Penrose_3)

    Penrose_4 <- all(round(G %*% A) == t(round(G %*% A)))
    message("GA = (GA)' is ", Penrose_4)

    return(all(Penrose_1, Penrose_2, Penrose_3, Penrose_4))
  }
}

#' Inverse of a Square Matrix
#'
#' This function calculates the inverse of a square matrix.
#'
#' @param A A square numeric matrix.
#'
#' @return The inverse of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' inverse(A)
inverse <- function(A) {
  # browser()
  # Check for squareness
  if (nrow(A) != ncol(A)) {
    message("Matrix is not invertible")
    return(NULL)
  }

  # check for singularity
  if (det(A) == 0) {
    message("Matrix is not invertible")
    return(NULL)
  }

  # invert!
  adjA <- adjugate(A)
  return(adjA * det(A)^-1)
}

#' Orthogonalize a Matrix
#'
#' This function orthogonalizes a matrix by computing the eigenvectors of `A %*% t(A)` or `t(A) %*% A`.
#'
#' @param A A numeric matrix.
#'
#' @return An orthogonalized matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' orthogonalize(A)
orthogonalize <- function(A) {
  preQ <- A %*% t(A)
  Q <- eigen(preQ, symmetric = TRUE)$vectors
  for (row in seq_len(nrow(Q))) {
    # Normalize Rows
    Q[row, ] <- 1 / sqrt(c(Q[row, ] %*% Q[row, ])) * c(Q[row, ])
  }
  return(Q)
}

# Rank --------------------------------------------------------------------

#' Find the Rank of a Square Matrix
#'
#' This function calculates the rank of a square matrix by counting the number of non-zero eigenvalues.
#'
#' @param A A square numeric matrix.
#' @param tol A numeric value specifying the tolerance for determining if an eigenvalue is non-zero. Defaults to `1e-12`.
#'
#' @return The rank of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' rank_square_matrix(A)
rank_square_matrix <- function(A, tol = 1e-12) {
  rank_sq <- sum(abs(Re(eigen(A)$values)) > tol) # Re trims imaginary part from eigen
  return(rank_sq)
}

#' Find the Rank of a Matrix
#'
#' This function calculates the rank of a matrix by converting it to row echelon form and counting the number of non-zero rows.
#'
#' @param A A numeric matrix.
#'
#' @return The rank of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' rank_matrix(A)
rank_matrix <- function(A) {
  A <- ref(A)
  zeroVecsIdx <- find_zero_vectors(A)

  if (length(zeroVecsIdx) == 0) {
    return(nrow(A))
  } else {
    return(nrow(A[-zeroVecsIdx, , drop = FALSE]))
  }
}

#' Singular Value Decomposition
#'
#' This function performs a singular value decomposition of a matrix `A`.
#'
#' @param A A numeric matrix.
#'
#' @return A list with the matrices `P`, `D`, and `Q`, representing the singular value decomposition of `A`.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' singular_value_decomposition(A)
singular_value_decomposition <- function(A) {
  # create P (left signular vectors)
  P <- orthogonalize(A)

  # create Q (right-singular vectors)
  Q <- orthogonalize(t(A))

  # get eigenvalues
  eigenVal <- eigen(A %*% t(A))$values

  # create diagonal matrix of singular values
  # D <- matrix(0,nrow = length(eigenVal), ncol = length(eigenVal))
  D <- matrix(0, nrow = length(eigenVal), ncol = length(eigenVal) + (ncol(A) - nrow(A)))
  # zero padding adapted from ISBN 978-1-118-93514-9, p.154

  # Since AA^T or A^TA have strictly non-negative eigenvalues, as they equal
  # the square of the eigenvalues of A, we take the abs value before, calculating
  # the sqrt of the eigenvalues.

  diag(D) <- sqrt(abs(eigenVal))

  # output generation
  res <- list(P = P, D = D, Q = Q)

  message("Warning, the sign ambiguity of singular vectors has not yet been solved!!!")

  return(res)
}

#' Get Row Echelon Form of a Matrix
#'
#' This function converts a matrix to its row echelon form using Gaussian elimination.
#'
#' @param A A numeric matrix.
#'
#' @return The row echelon form of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' ref(A)
ref <- function(A) {
  # Corner Case 1: all zero matrix
  if (max(abs(A)) == 0) {
    return(A)
  }

  # 1.) Setup variables
  nc <- ncol(A)
  # TODO: is this
  # nr <- nrow(A)

  # first pivot must be on first column
  currentCol <- 1
  idxPivots <- data.frame(i = 1, j = 1)

  # 1.1.) remove parallel vectors
  rmIdx <- unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)

  # 1.2.) set small values to zero
  A <- ifelse(abs(A) <= 1e-10, 0, A)

  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)) {
    A[rmIdx, ] <- rep(0, nc)
    A <- add_to_bottom(A, rmIdx)
  }

  # 2.) find first pivot (if exists, else normalize first row to first element)
  firstPivotIdx <- which(A[, 1] == 1)[1]

  # check if a pivot exists?
  if (!is.na(firstPivotIdx)) {
    A <- swap(A, firstPivotIdx, 1)
  } else {
    # Create a pivot (normalize first row)
    A[1, ] <- (1 / A[1, 1]) * A[1, ]
  }

  # 2.1.) Check if column are all 0 except for the row with the pivot
  if (!col_is_all_zero(A, currentCol = currentCol)) {
    # find nonzero elements in col. vector
    idxNonzero <- which(A[-1, currentCol] != 0) + currentCol

    # Perform Elementary Row OPs
    A <- gaussian_elimination(A, idxNonzero, currentCol)
  }

  # 3.) find potential next pivots and swap rows if exist
  currentCol <- 2
  currentRow <- 1

  # make loop even easier
  for (col in currentCol:nc) {
    # 3.2.) Reorganize matrix such that zero vectors are at the bottom, rm parallel vectors
    A <- swap_zero_vectors(A)
    A <- remove_parallel_vectors(A)

    # skip if col is zero
    if (all(A[-c(1:currentRow), col] == 0)) {
      next
    }

    # go one row down to follow the diagonal if the column is not full of zeros
    currentRow <- currentRow + 1


    # create new pivot
    A[currentRow, ] <- (1 / A[currentRow, col]) * A[currentRow, ]

    # add pivot index
    idxPivots <- rbind(idxPivots, c(currentRow, col))

    # 3.1.) Check if column are all 0 except for the row with the pivot
    if (!col_is_all_zero(A, currentCol = col)) {
      # find nonzero elements in col. vector
      idxNonzero <- which(A[-c(1:currentRow), col] != 0) + col

      # Perform Elementary Row OPs
      A <- gaussian_elimination(A, idxNonzero, col)
    }
  }
  return(A)
}

#' Get Reduced Row Echelon Form of a Matrix
#'
#' This function converts a matrix to its reduced row echelon form using Gaussian elimination.
#'
#' @param A A numeric matrix.
#'
#' @return The reduced row echelon form of the matrix.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' rref(A)
rref <- function(A) {
  # Corner Case 1: all zero matrix
  if (max(abs(A)) == 0) {
    return(A)
  }

  # 1.) Setup variables
  nc <- ncol(A)
  nr <- nrow(A)

  # first pivot must be on first column
  currentCol <- 1
  idxPivots <- data.frame(i = 1, j = 1)

  # 1.1.) remove parallel vectors
  rmIdx <- unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)

  # # 1.2.) set small values to zero
  # ifelse(abs(A) <= 1e-10, 0, A)

  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)) {
    A[rmIdx, ] <- rep(0, nc)
    A <- add_to_bottom(A, rmIdx)
  }

  # 2.) find first pivot (if exists, else normalize first row to first element)
  firstPivotIdx <- which(A[, 1] == 1)[1]

  # check if a pivot exists?
  if (!is.na(firstPivotIdx)) {
    A <- swap(A, firstPivotIdx, 1)
  } else {
    # Create a pivot (normalize first row)
    A[1, ] <- (1 / A[1, 1]) * A[1, ]
  }

  # 2.1.) Check if column are all 0 except for the row with the pivot
  if (!col_is_all_zero(A, currentCol = currentCol)) {
    # find nonzero elements in col. vector
    idxNonzero <- which(A[-1, currentCol] != 0) + currentCol

    # Perform Elementary Row OPs
    A <- gaussian_elimination(A, idxNonzero, currentCol)
  }

  # 3.) find potential next pivots and swap rows if exist
  currentCol <- 2
  currentRow <- 1

  # make loop even easier
  for (col in currentCol:nc) {
    # 3.2.) Reorganize matrix such that zero vectors are at the bottom, rm parallel vectors
    A <- swap_zero_vectors(A)
    A <- remove_parallel_vectors(A)


    # skip if col is zero
    if (all(A[-c(1:currentRow), col] == 0)) {
      next
    }

    # go one row down to follow the diagonal if the column is not full of zeros
    currentRow <- currentRow + 1

    # create new pivot
    A[currentRow, ] <- (1 / A[currentRow, col]) * A[currentRow, ]

    # add pivot index
    idxPivots <- rbind(idxPivots, c(currentRow, col))

    # 3.1.) Check if column are all 0 except for the row with the pivot
    if (!col_is_all_zero(A, currentCol = col)) {
      # find nonzero elements in col. vector
      idxNonzero <- which(A[-c(1:currentRow), col] != 0) + col

      # Perform Elementary Row OPs
      A <- gaussian_elimination(A, idxNonzero, col)
    }
  }

  # 4.) Now remove all free variable above a pivot

  # create matrix containing only the pivots
  pivotsVec <- A[idxPivots$i, , drop = FALSE]

  #
  nr <- nrow(pivotsVec)

  # this loop starts with the "lowest" pivot vector (piv) (the one with rightmost pivot)
  # and subtracts itself from the next pivot (revPiv) vector such that the free variable
  # above the current pivot vector (piv).
  for (piv in nr:1) {
    message("piv: ", piv)
    for (revPiv in piv:1) {
      message("revPiv: ", revPiv)
      if (piv == revPiv) {
        next
      }
      A[revPiv, ] <- A[revPiv, ] - A[piv, ] * A[revPiv, piv]
    }
  }

  # 5.) set small values to zero
  A <- ifelse(abs(A) <= 1e-10, 0, A)

  return(A)
}

# Plotting Functions ------------------------------------------------------

#' Plot Eigenvectors of a 2x2 Matrix
#'
#' This function visualizes the eigenvectors of a 2x2 matrix, showing the transformation of the basis vectors and the span of the eigenvectors.
#'
#' @param A A 2x2 numeric matrix.
#' @param offset A numeric value specifying the offset for the plot limits. Defaults to `1`.
#' @param plotBasisVecs A logical value indicating whether to plot the basis vectors. Defaults to `TRUE`.
#' @param plotSpan A logical value indicating whether to plot the span of the vectors. Defaults to `TRUE`.
#' @param plotTransBasis A logical value indicating whether to plot the transformed basis vectors. Defaults to `TRUE`.
#'
#' @return A plot of the eigenvectors.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' plot_eigenvec(A)
plot_eigenvec <- function(A,
                          offset = 1,
                          plotBasisVecs = TRUE,
                          plotSpan = TRUE,
                          plotTransBasis = TRUE) {
  # assumptions for function:
  if (all(dim(A) != 2)) {
    message("A is not 2x2. Exiting...")
    return(NULL)
  }

  par(pty = "s")

  maxMat <- max(A) + offset

  plot(1,
    type = "n",
    xlab = "", ylab = "",
    xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
    main = paste("Eigenvalues = ", eigen(A)$values), cex.main = 0.75
  )
  grid()

  # basis vectors
  if (plotBasisVecs) {
    arrows(0, 0, 0, 1, length = 0.05)
    arrows(0, 0, 1, 0, length = 0.05)
  }

  # span basis vectors
  arrows(0, -1 * 2 * maxMat, 0, 1 * 2 * maxMat,
    length = 0.05, col = rgb(0, 0, 0, 0.3), code = 0, lty = 2
  )
  arrows(-1 * 2 * maxMat, 0, 1 * 2 * maxMat, 0,
    length = 0.05, col = rgb(0, 0, 0, 0.3), code = 0, lty = 2
  )

  # transformed basis vectors
  if (plotTransBasis) {
    arrows(0, 0, A[1, 1], A[2, 1], length = 0.05, col = rgb(0, 0, 0.8, 0.7))
    arrows(0, 0, A[1, 2], A[2, 2], length = 0.05, col = rgb(0, 0, 0.8, 0.7))
  }

  # span transformed basis vectors
  if (plotSpan) {
    arrows(-A[1, 1] * maxMat, -A[2, 1] * maxMat,
      A[1, 1] * maxMat, A[2, 1] * maxMat,
      length = 0.05, col = rgb(0, 0, 0.8, 0.3), code = 0, lty = 2
    )
    arrows(-A[1, 2] * maxMat, -A[2, 2] * maxMat,
      A[1, 2] * maxMat, A[2, 2] * maxMat,
      length = 0.05, col = rgb(0, 0, 0.8, 0.3), code = 0, lty = 2
    )
  }


  # eigenvectors
  eign <- eigen(A)

  if (any(Im(eign$values) != 0)) {
    message("Some eigenvalues are complex, but only real values are considered!\n")
  }

  # imagenary part of eigenvectors is stripped
  eigenVec_1 <- Re(eign$vectors[, 1]) * Re(eign$values[1])
  eigenVec_2 <- Re(eign$vectors[, 2]) * Re(eign$values[2])

  # eigenvectors plotting
  arrows(0, 0, eigenVec_1[1], eigenVec_1[2], length = 0.05, col = rgb(0.8, 0, 0.8, 0.7))
  arrows(0, 0, eigenVec_2[1], eigenVec_2[2], length = 0.05, col = rgb(0.8, 0, 0, 0.7))

  # span eigenvectors plotting
  if (plotSpan) {
    arrows(-eigenVec_1[1] * maxMat, -eigenVec_1[2] * maxMat,
      eigenVec_1[1] * maxMat, eigenVec_1[2] * maxMat,
      length = 0.05, col = rgb(0.8, 0, 0.8, 0.3), lty = 2
    ) # aes
    arrows(-eigenVec_2[1] * maxMat, -eigenVec_2[2] * maxMat,
      eigenVec_2[1] * maxMat, eigenVec_2[2] * maxMat,
      length = 0.05, col = rgb(0.8, 0, 0, 0.3), lty = 2
    ) # aes
  }

  # plot origin
  points(0, 0, pch = 16, cex = 0.7, )

  # legend
  legend("topleft", c("Basis", "Transformed Basis", "Scaled Eigenvectors"),
    col = c(rgb(0, 0, 0, 1), rgb(0, 0, 0.8, 0.7), rgb(0.8, 0, 0.8, 0.7)),
    pch = c(16, 16, 16),
    inset = c(1, 0), xpd = TRUE, horiz = FALSE, bty = "n",
    cex = 0.75
  )

  # restore par settings to default
  par(mfrow = c(1, 1))
}

#' Plot Matrix Transformation
#'
#' This function visualizes the transformation of a vector `v` by a 2x2 matrix `A`.
#'
#' @param A A 2x2 numeric matrix.
#' @param v A numeric vector.
#' @param offset A numeric value specifying the offset for the plot limits. Defaults to `1`.
#' @param plotBasisVecs A logical value indicating whether to plot the basis vectors. Defaults to `TRUE`.
#' @param splitPlot A logical value indicating whether to create a split plot showing the transformation before and after. Defaults to `TRUE`.
#'
#' @return A plot of the matrix transformation.
#' @export
#'
#' @examples
#' A <- matrix(c(2, -1, -1, 2), nrow = 2)
#' v <- c(1, 1)
#' plot_matrix_transformation(A, v)
plot_matrix_transformation <- function(A, v,
                                       offset = 1,
                                       plotBasisVecs = TRUE,
                                       splitPlot = TRUE) {
  # assumptions for function:
  if (all(dim(A) != 2)) {
    message("A is not 2x2. Exiting...")
    return(NULL)
  }

  par(mfrow = c(1, 2), pty = "s")

  maxMat <- max(A) + offset

  plot(1,
    type = "n",
    xlab = "", ylab = "",
    xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
    main = "Before Transformation", cex.main = 0.75
  )
  grid()

  # basis vectors
  if (plotBasisVecs) {
    arrows(0, 0, 0, 1, length = 0.05)
    arrows(0, 0, 1, 0, length = 0.05)
  }

  # span basis vectors
  arrows(0, -1 * 2 * maxMat, 0, 1 * 2 * maxMat,
    length = 0.05, col = rgb(0, 0, 0, 0.3), code = 0, lty = 2
  )
  arrows(-1 * 2 * maxMat, 0, 1 * 2 * maxMat, 0,
    length = 0.05, col = rgb(0, 0, 0, 0.3), code = 0, lty = 2
  )

  # input vector
  arrows(0, 0, v[1], v[2], length = 0.05, col = rgb(0.8, 0, 0, 0.8))
  text(v[1], v[2] + round(offset / 2), paste("[", v[1], v[2], "]"),
    col = rgb(0.8, 0, 0, 0.8), cex = 0.75
  )

  # plot origin and tip of vec
  points(0, 0, pch = 16, cex = 0.7, )
  points(0, 0, pch = 16, cex = 0.7, )

  # legend
  legend("bottomright", c("Basis", "Vector x (Ax=y)"),
    col = c(rgb(0, 0, 0.8, 0.7), rgb(0.8, 0, 0.8, 0.7)),
    pch = c(16, 16), inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n",
    cex = 0.75
  )

  # second plot with transformation

  # find new boundaries
  v_trans <- A %*% v
  if (max(v_trans) > maxMat) {
    maxMat <- max(v_trans) + offset
  }

  if (splitPlot) {
    # emtpy plot
    plot(1,
      type = "n",
      xlab = "", ylab = "",
      xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
      main = "After Transformation", cex.main = 0.75
    )

    # formula for getting angle between vectors
    angle <- function(v1, v2) {
      acos(sum(v1 * v2) / (sqrt(sum(v1 * v1)) * sqrt(sum(v2 * v2))))
    }

    # find angle
    angle1 <- angle(c(1, 0), A[, 1])
    angle2 <- -angle(c(0, 1), A[, 2])


    # apply grid (swipe through grid in intervals (seq) and draw abline with slope = angle)
    sapply(seq(-maxMat * 10, maxMat * 10, by = 1), function(inter) {
      abline(
        a = inter,
        b = tan(angle1),
        lty = 3,
        col = "lightgray"
      )
    })
    sapply(seq(-maxMat * 10, maxMat * 10, by = 4), function(inter) {
      abline(
        a = inter,
        b = tan(angle2 + pi / 2),
        lty = 3,
        col = "lightgray"
      )
    })
  }


  # transformed basis vectors
  if (plotBasisVecs) {
    arrows(0, 0, A[1, 1], A[2, 1], length = 0.05, col = rgb(0, 0, 0.8, 0.7))
    arrows(0, 0, A[1, 2], A[2, 2], length = 0.05, col = rgb(0, 0, 0.8, 0.7))
  }

  # span transformed basis vectors

  arrows(-A[1, 1] * maxMat, -A[2, 1] * maxMat,
    A[1, 1] * maxMat, A[2, 1] * maxMat,
    length = 0.05, col = rgb(0, 0, 0.8, 0.3), code = 0, lty = 2
  )
  arrows(-A[1, 2] * maxMat, -A[2, 2] * maxMat,
    A[1, 2] * maxMat, A[2, 2] * maxMat,
    length = 0.05, col = rgb(0, 0, 0.8, 0.3), code = 0, lty = 2
  )



  # input vector transformed
  arrows(0, 0, v_trans[1], v_trans[2], length = 0.05, col = rgb(0.5, 0, 0.8, 0.8))
  text(v_trans[1], v_trans[2] + round(offset / 2), paste("[", v_trans[1], v_trans[2], "]"),
    col = rgb(0.5, 0, 0.8, 0.8), cex = 0.75
  )

  # legend
  legend("bottomright", c("Basis", "Vector y (Ax=y)"),
    col = c(rgb(0, 0, 0.8, 0.7), rgb(0.8, 0, 0.8, 0.7)),
    pch = c(16, 16), inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n",
    cex = 0.75
  )

  # restore par settings to default
  par(mfrow = c(1, 1))
}

# Auxillary Functions -----------------------------------------------------

#' Compare two floats
#'
#' @param a A float.
#' @param b A float.
#' @param tol The tolerance for the comparison.
#'
#' @return A logical indicating whether the floats are equal within the tolerance.
#' @export
#'
#' @examples
#' compare_floats(1.000001, 1)
compare_floats <- function(a, b, tol = 1e-06) {
  return(abs(a - b) < tol)
}

#' Swap rows or columns of a matrix
#'
#' @param A A numeric matrix.
#' @param old The index of the row/column to swap.
#' @param new The index of the row/column to swap with.
#' @param col A logical indicating whether to swap columns instead of rows.
#'
#' @return The matrix with the rows/columns swapped.
#' @export
#'
#' @examples
#' A <- matrix(1:4, nrow = 2)
#' swap(A, 1, 2)
swap <- function(A, old, new, col = TRUE) {
  tmp <- A[old, ]
  A[old, ] <- A[new, ]
  A[new, ] <- tmp
  return(A)
}

#' Add rows to the bottom of a matrix
#' Used in rref.
#' @param A A numeric matrix.
#' @param to_append A vector of row indices to append to the bottom of the matrix.
#'
#' @return The matrix with the specified rows moved to the bottom.
#' @export
#'
#' @examples
#' A <- matrix(1:9, nrow = 3)
#' add_to_bottom(A, 1)
add_to_bottom <- function(A, to_append) {
  out <- rbind(A[-to_append, ], A[to_append, ])
  return(out)
}

#' Check if a column is all zero below the pivot
#' Used in rref.
#' @param A A numeric matrix.
#' @param currentCol The current column.
#'
#' @return A logical indicating whether the column is all zero below the pivot.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 1), nrow = 2)
#' col_is_all_zero(A, 1)
col_is_all_zero <- function(A, currentCol) {
  check <- all(A[-c(1:currentCol), currentCol] == 0)
  return(check)
}

#' Perform Gaussian elimination
#' Used in rref.
#' @param A A numeric matrix.
#' @param idxNonzero A vector of non-zero indices.
#' @param currentCol The current column.
#'
#' @return The matrix after Gaussian elimination.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' gaussian_elimination(A, 2, 1)
gaussian_elimination <- function(A, idxNonzero, currentCol) {
  for (ele in idxNonzero) {
    A[ele, ] <- A[ele, ] - A[ele, currentCol] * A[currentCol, ]
  }
  return(A)
}

#' Find zero vectors in a matrix
#'
#' @param A A numeric matrix.
#'
#' @return A vector of indices of the zero vectors.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 0), nrow = 2)
#' find_zero_vectors(A)
find_zero_vectors <- function(A) {
  idx <- which(rowSums(sqrt(A^2)) == 0)
  return(idx)
}

#' Swap zero vectors to the bottom of a matrix
#' Used to put zero vectors at the bottom if found in matrix.
#' @param A A numeric matrix.
#'
#' @return The matrix with the zero vectors at the bottom.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 0), nrow = 2, byrow = TRUE)
#' swap_zero_vectors(A)
swap_zero_vectors <- function(A) {
  zeroVecIdx <- find_zero_vectors(A)

  if (length(zeroVecIdx) == 0) {
    return(A)
  } else {
    return(add_to_bottom(A, zeroVecIdx))
  }
}

#' Remove parallel vectors from a matrix
#'
#' @param A A numeric matrix.
#'
#' @return The matrix with parallel vectors removed.
#' @export
#'
#' @examples
#' A <- matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE)
#' remove_parallel_vectors(A)
remove_parallel_vectors <- function(A) {
  # find parallel vectors
  rmIdx <- unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)

  nc <- ncol(A)
  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)) {
    A[rmIdx, ] <- rep(0, nc)
    A <- add_to_bottom(A, rmIdx)
    return(A)
  } else {
    return(A)
  }
}

#' Flip the sign of a scalar
#'
#' @param scalar A scalar.
#'
#' @return The scalar with the sign flipped.
#' @export
#'
#' @examples
#' flip_sign(-5)
flip_sign <- function(scalar) {
  if (scalar > 0) {
    return(scalar)
  } else {
    return(-1 * scalar)
  }
}
