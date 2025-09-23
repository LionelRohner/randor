
# Linear Algebra Functions Nobody Asked For -------------------------------


# None of the functions below have been thourougly tested!


# Main Functions ----------------------------------------------------------

#' Title
#' Create linear independent vectors aka a basis 
#' @param dim 
#' @param negative 
#' @param upper 
#' @param returnMat 
#'
#' @return
#' @export
#'
#' @examples
create_basis <- function(dim, negative = T, upper = 9, returnMat = F){
  
  if (negative){
    lower = -upper
  } else {
    lower = 0
  }
  
  basis = matrix(sample(lower:upper,dim^2,replace =T), nrow = dim)
  
  while (det(basis) == 0){
    basis = matrix(sample(lower:upper,dim^2,replace =T), nrow = dim)
  }
  
  if (returnMat){
    return(basis) # return in matrix form
  }
  
  # output list of vecs instead of matrix
  out = list()
  for (rowcol in 1:dim){
    out[[rowcol]] = basis[,rowcol]
  }
  return(out)
}

#' Title
#' Is Positive Definite?
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
is_pos_def <- function(A){# test 1 - symmetry
  
  # test 1 - is symmetric?
  test1 = all(round(A, 5) == round(t(A),5))
  
  message("Test 1 - Matrix is symmetric? ", test1)
  
  # test 2 - positive eigenvalues
  test2 = prod(Re(eigen(A)$value)) > 0
  
  message("Test 2 - All (real) eigenvalues are positive? ", test2)
  
  # test 3 - positive upper left submatrices
  dimA = nrow(A)
  upLeftDets = c(A[1,1])
  if (A[1,1] < 0){
    test3 = FALSE
  } else { 
    for (i in 2:dimA){
      upLeftDet = det(A[1:i,1:i])
      if (upLeftDet < 0){
        test3 = FALSE
      } else {
        upLeftDets = c(upLeftDets,det(A[1:i,1:i]))
      }
    }
  }
  test3 = all(upLeftDets > 0)
  
  message("Test 3 - Determinant of upper left submatrices are > 0? ", test3)
  
  # output  
  return(all(test1,test2,test3))
}

#' Title
#' Create canonical form (A = UDU^-1)
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
canonical_form <- function(A){
  # create UDU
  
  # create U
  eign = eigen(A)
  U = eign$vectors
  
  # create D
  D <- matrix(0,nrow = length(eign$values), ncol = length(eign$values))
  diag(D) = eign$values
  
  # create U-1
  U_inv = adjugate(U)*1/det(U)
  
  # output
  res <- list(U = U, D = D, U_inv = U_inv)
  return(res) 
}

# TODO: CHECK IF A IS DEFECTIVE, AS OTHERWISE U-1 DOES NOT EXIST!!

#' Title
#' Fast exponentiation using canonical form
#' @param A 
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
fast_exp <- function(A,p){
  UDU = canonical_form(A)
  
  # fast exponentiation of eigenvalues
  D <- UDU$D
  D_to_the_p = diag(D)^p
  
  # calculate UD^pU^-1
  # U doesnt have to be included in the power calc, because A = UDU^-1, thus A^2 = UDU^-1UDU^-1, which is UDDU^-1
  diag(D) = D_to_the_p
  
  UDU_inv <- UDU$U%*%D%*%UDU$U_inv
  
  return(UDU_inv)
}

#' Title
#' Calculate matrix powers
#' @param A 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
mat_pow <- function(A, n){
  # TODO: Cheap workaround...
  n = n-1
  
  if(n == 0){
    return(A)
  } else if (n == -1){
    # return identity matrix
    I <- matrix(0,nrow = nrow(A), ncol = ncol(A))
    diag(I) = rep(1,nrow(A))
    return(I)
  }
  
  A_to_the_i <- A
  for (i in 1:n){
    A_to_the_i <- A%*%A_to_the_i
  }; return(A_to_the_i)
}


#  ---------------------------------------

#' Title
#' Single Cautchy-Schwartz inequality test. Check two vectors
#' @param u 
#' @param v 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
lin_dep_Cautchy_Schwartz <- function(u,v, tol = 1e-05){
  # Consists of checking whether <u,v> >= ||u|| ||v||
  # Strict equality indicate linear dependence, i.e. <u,v> = ||u|| ||v||
  
  # compute inner prod u^Tu and v^Tv
  uu = u %*% u
  vv = v %*% v
  
  # compute inner prod u^Tv
  uv_squared = (u %*% v)^2
  
  # check for strict equality to find lin. dependent rows/cols
  if (compare_floats(uu*vv,uv_squared,tol = tol)){
    message("Vectors are linearly dependent!")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Title
#' Cautchy-Schwartz-Inequality to identify parallel cols/rows. Check for more than
#' two vecs
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
lin_dep_Cautchy_Schwartz_matrix <- function(A){
  # Consists of checking whether <u,v> >= ||u|| ||v||
  # Strict equality indicate linear dependence, i.e. <u,v> = ||u|| ||v||
  
  # TODO: What about the commented out block below?
  # # Check whether there are more rows or cols, then choose shorter space
  # # This reduces the # of iterations in the double loop
  # if(ncol(A) > nrow(A)){
  #   A = t(A)
  # } 
  
  # output container
  linDepIdx = c()
  
  # Cauchy-Schwarzt Loop
  for (i in 1:nrow(A)){
    
    # skip if the vector is a zero vector
    if (all(A[i,] == 0)){
      next
    }
    
    for (j in i:nrow(A)){
      
      # skip if the vector is a zero vector
      if (all(A[j,] == 0)){
        next
      }
      
      if (i != j){
        
        # compute norms and dot prod of the span of the row/col space
        u_norm = sqrt(sum(A[i,]^2))
        v_norm = sqrt(sum(A[j,]^2))
        u_v = A[i,] %*% A[j,]
        
        # check for strict equality to find lin. dependent rows/cols
        if (compare_floats(u_v,u_norm * v_norm)){
          # mirrored duplicates should be excluded
          linDepIdx = rbind.data.frame(linDepIdx,c(i,j)) 
        }
      }
    }
  }
  
  # output construction
  if (is.null(nrow(linDepIdx))){
    # message("No linear dependent cols/rows were found!")
    return(NULL)
  } else {
    colnames(linDepIdx) = c("i","j")
    # message("Linear combinations of cols/rows were found!")
    return(linDepIdx)
  }
}

#' Title
#' Create an adjugate matrix (transpose of a cofactor matrix)
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
adjugate <- function(A){
  
  n = nrow(A)
  m = ncol(A)
  
  if (n != m){
    message("Matrix must be square!")
    return(NULL)
  }
  
  # create emtpy cofactor matrix
  C = matrix(NA,nrow = n, ncol = m)
  
  # special case for A of order 2x2
  if (n == 2){
    C = (-1)*A
    diag(C) = rev(diag(A))
    return(t(C))
  }
  
  # populate the cofactor matrix
  for (i in 1:n){
    for (j in 1:m){
      C[i,j] = (-1)^(i+j)*det(A[-i,-j])
    }
  }
  
  return(t(C))
}


# TODO: FIX LIMITATIONS
# does only work for obvious cases of lin. dep. (i.e. parallel vectors) that are 
# identified by the Cautchy-Schwartz Inequality (e.g. [2,4] = 2*[1,2]), but not
# if lin. dep. arise from combinations of vectors (e.g. [4,0] = [2,-2] + [2,2]).

#' Title
#' Create generalized inverse from a mxn matrix
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
generalized_inverse <- function(A){
  
  ### step 1 : Find a LIN submatrix of order rxr 
  message("Step 1 : Find a LIN submatrix W of order rxr in A!")
  
  nc = ncol(A)
  nr = nrow(A)
  
  rank = rank_matrix(A)
  
  # Check whether A is full rank anyways
  if (nc == nr && det(A) != 0){
    W = A
  } else   if (rank == 1){
    idxRow = idxCol = 1
    W = A[idxRow,idxCol,drop=FALSE]
  } else {
    
    # make matrix square
    outerBreak = FALSE
    for (i in 1:(nr-rank+1)){
      for (j in 1:(nc-rank+1)){
        print(i:(i+rank-1))
        print(j:(j+rank-1))
        print(A[i:(i+rank-1),j:(j+rank-1)])
        print(det(A[i:(i+rank-1),j:(j+rank-1)]))
        if (det(A[i:(i+rank-1),j:(j+rank-1)]) != 0){
          idxRow = c(i:(i+rank-1))
          idxCol = c(j:(j+rank-1))
          outerBreak = TRUE 
          break
        }
        if(outerBreak){
          break
        }
      }
    }
    
    if (!outerBreak){
      message("All submatrices are singular! Generalized Inverse could not be calculated!")
      return(NULL)
    }
    
    W <- A[idxRow, idxCol]
  }
  
  
  ### step 2 : (W^-1)^T
  message("Step 2 : (W^-1)^T!")
  
  # create adjoint matrix - adjoint() doesnt work for 2x2
  if (dim(W)[1] == 2){
    adj_W = -1*W
    diag(adj_W) = rev(diag(W))
  } else {
    adj_W = adjugate(W)
  }
  
  transp_inv_W <- t(adj_W/det(W))
  
  ### step 3 : replace elements of A with (W^-1)^T
  message("Step 3 : Project rows/cols of (W^-1)^T into a a zero matrix A0 of order dim(A)!")
  
  # Again, if A is non-singular from scratch skip Step 3
  if (nc == nr && det(A) != 0){
    A0 = transp_inv_W
  } else{
    A0 <- 0*A
    A0[idxRow,idxCol] = transp_inv_W
    
  }
  
  
  ### step 4 : t(A)
  message("Step 3 : G = A0^T!")
  
  G <- t(A0)
  
  return(G)
}

#' Title
#' Check Penrose Condition of Inverses
#' @param A 
#' @param G 
#' @param all_Penrose_check 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
check_Penrose_cond <- function(A,
                               G,
                               all_Penrose_check = F,
                               digits = 2){
  
  message("Disclaimer: All matrices are transformed to pure integer matrices first, so consider that...\n")
  
  if (all_Penrose_check == F){
    Penrose_1 = all(round(A%*%G%*%A,2) == A)
    message("AGA=A is ", Penrose_1)
    return(Penrose_1)
  }else{
    Penrose_1 = all(round(A%*%G%*%A) == A)
    message("AGA = A is ", Penrose_1)
    
    Penrose_2 = all(round(G%*%A%*%G,digits = digits) == round(G,digits = digits))
    message("GAG = G is ", Penrose_2)
    
    Penrose_3 = all(round(A%*%G) == t(round(A%*%G)))
    message("AG = (AG)' is ", Penrose_3)
    
    Penrose_4 = all(round(G%*%A) == t(round(G%*%A)))
    message("GA = (GA)' is ", Penrose_4)
    
    return(all(Penrose_1,Penrose_2,Penrose_3,Penrose_4))
  }
}

#' Title
#' Inverse of a square matrix
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
inverse <- function(A){
  # Check for squareness
  if (nrow(A) != ncol(A)){
    message("Matrix is not invertible")
    return(NULL)
  }
  
  # check for singularity
  if (det(A) == 0){
    message("Matrix is not invertible")
    return(NULL)
  }
  
  # invert!
  adjA = adjugate(A)
  return(adjA*det(A)^-1)
}

#' Title
#' Orthogonalize matrix (AAT or ATA)
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
orthogonalize <- function(A){
  preQ = A%*%t(A)
  Q = eigen(preQ, symmetric = T)$vectors
  for (row in 1:nrow(Q)){
    # Normalize Rows
    Q[row,] = 1/sqrt(c(Q[row,]%*%Q[row,]))*c(Q[row,])
  }
  return(Q)
}

# Rank --------------------------------------------------------------------

#' Title
#' Find the rank of a square matrix
#' @param A 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
rank_square_matrix <- function(A, tol = 1e-12){
  rank_sq <- sum(abs(Re(eigen(A)$values)) > tol) # Re trims imaginary part from eigen
  return(rank_sq)
}

#' Title
#'
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
rank_matrix <- function(A){
  A = ref(A)
  zeroVecsIdx = find_zero_vectors(A)
  
  if (length(zeroVecsIdx) == 0){
    return(nrow(A))
  } else {
    return(nrow(A[-zeroVecsIdx,,drop=FALSE]))
  }
}

#' Title
#' Singular value decomposition
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
singular_value_decomposition <- function(A){
  # create P (left signular vectors)
  P <- orthogonalize(A)
  
  # create Q (right-singular vectors)
  Q <- orthogonalize(t(A))
  
  # get eigenvalues
  eigenVal <- eigen(A%*%t(A))$values
  
  # create diagonal matrix of singular values
  # D <- matrix(0,nrow = length(eigenVal), ncol = length(eigenVal))
  D <- matrix(0,nrow = length(eigenVal), ncol = length(eigenVal) + (ncol(A) - nrow(A)))
  # zero padding adapted from ISBN 978-1-118-93514-9, p.154
  
  # Since AA^T or A^TA have strictly non-negative eigenvalues, as they equal
  # the square of the eigenvalues of A, we take the abs value before, calculating
  # the sqrt of the eigenvalues.
  
  diag(D) <- sqrt(abs(eigenVal))
  
  # output generation
  res <- list(P = P,D = D, Q = Q)
  
  message("Warning, the sign ambiguity of singular vectors has not yet been solved!!!")
  
  return(res)
}

#' Title
#' Get row echelon form of a amtrix
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
ref <- function(A){
  
  # Corner Case 1: all zero matrix
  if (max(abs(A)) == 0){
    return(A)
  }
  
  # 1.) Setup variables
  nc = ncol(A)
  nr = nrow(A)
  
  # first pivot must be on first column
  currentCol = 1
  idxPivots = data.frame(i = 1, j = 1)
  
  # 1.1.) remove parallel vectors
  rmIdx = unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)
  
  # 1.2.) set small values to zero
  A = ifelse(abs(A) <= 1e-10, 0, A)
  
  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)){
    A[rmIdx,] <- rep(0,nc)
    A <- add_to_bottom(A,rmIdx)
  } 
  
  # 2.) find first pivot (if exists, else normalize first row to first element)
  firstPivotIdx = which(A[,1]==1)[1]
  
  # check if a pivot exists?
  if (!is.na(firstPivotIdx)){
    A <- swap(A,firstPivotIdx,1)
    
  } else {
    # Create a pivot (normalize first row)
    A[1,] = (1/A[1,1]) * A[1,]
  }
  
  # 2.1.) Check if column are all 0 except for the row with the pivot
  if (!col_is_all_zero(A,currentCol = currentCol)){
    
    # find nonzero elements in col. vector
    idxNonzero = which(A[-1,currentCol]!=0) + currentCol
    
    # Perform Elementary Row OPs
    A = gaussian_elimination(A,idxNonzero,currentCol)
  } 
  
  # 3.) find potential next pivots and swap rows if exist
  currentCol = 2
  currentRow = 1
  
  # make loop even easier
  for (col in currentCol:nc){
    
    
    # 3.2.) Reorganize matrix such that zero vectors are at the bottom, rm parallel vectors
    A = swap_zero_vectors(A)
    A = remove_parallel_vectors(A)
    
    # skip if col is zero
    if (all(A[-c(1:currentRow),col] == 0)){
      next
    }
    
    # go one row down to follow the diagonal if the column is not full of zeros
    currentRow = currentRow + 1
    
    
    # create new pivot
    A[currentRow,] = (1/A[currentRow,col]) * A[currentRow,]
    
    # add pivot index
    idxPivots = rbind(idxPivots,c(currentRow, col))
    
    # 3.1.) Check if column are all 0 except for the row with the pivot
    if (!col_is_all_zero(A,currentCol = col)){
      
      # find nonzero elements in col. vector
      idxNonzero = which(A[-c(1:currentRow),col]!=0) + col
      
      # Perform Elementary Row OPs
      A = gaussian_elimination(A,idxNonzero,col)
    }
  }
  return(A)
}

#' Title
#' Get reduced row echelon form from a matrix A
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
rref <- function(A){
  
  # Corner Case 1: all zero matrix
  if (max(abs(A)) == 0){
    return(A)
  }
  
  # 1.) Setup variables
  nc = ncol(A)
  nr = nrow(A)
  
  # first pivot must be on first column
  currentCol = 1
  idxPivots = data.frame(i = 1, j = 1)
  
  # 1.1.) remove parallel vectors
  rmIdx = unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)
  
  # # 1.2.) set small values to zero
  # ifelse(abs(A) <= 1e-10, 0, A)
  
  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)){
    A[rmIdx,] <- rep(0,nc)
    A <- add_to_bottom(A,rmIdx)
  } 
  
  # 2.) find first pivot (if exists, else normalize first row to first element)
  firstPivotIdx = which(A[,1]==1)[1]
  
  # check if a pivot exists?
  if (!is.na(firstPivotIdx)){
    A <- swap(A,firstPivotIdx,1)
    
  } else {
    # Create a pivot (normalize first row)
    A[1,] = (1/A[1,1]) * A[1,]
  }
  
  # 2.1.) Check if column are all 0 except for the row with the pivot
  if (!col_is_all_zero(A,currentCol = currentCol)){
    
    # find nonzero elements in col. vector
    idxNonzero = which(A[-1,currentCol]!=0) + currentCol
    
    # Perform Elementary Row OPs
    A = gaussian_elimination(A,idxNonzero,currentCol)
  } 
  
  # 3.) find potential next pivots and swap rows if exist
  currentCol = 2
  currentRow = 1
  
  # make loop even easier
  for (col in currentCol:nc){
    
    # 3.2.) Reorganize matrix such that zero vectors are at the bottom, rm parallel vectors
    A = swap_zero_vectors(A)
    A = remove_parallel_vectors(A)
    
    
    # skip if col is zero
    if (all(A[-c(1:currentRow),col] == 0)){
      next
    }
    
    # go one row down to follow the diagonal if the column is not full of zeros
    currentRow = currentRow + 1
    
    # create new pivot
    A[currentRow,] = (1/A[currentRow,col]) * A[currentRow,]
    
    # add pivot index
    idxPivots = rbind(idxPivots,c(currentRow, col))
    
    # 3.1.) Check if column are all 0 except for the row with the pivot
    if (!col_is_all_zero(A,currentCol = col)){
      
      # find nonzero elements in col. vector
      idxNonzero = which(A[-c(1:currentRow),col]!=0) + col
      
      # Perform Elementary Row OPs
      A = gaussian_elimination(A,idxNonzero,col)
    }
  }
  
  # 4.) Now remove all free variable above a pivot
  
  # create matrix containing only the pivots
  pivotsVec <- A[idxPivots$i,,drop = FALSE]
  
  # 
  nr = nrow(pivotsVec)
  
  # this loop starts with the "lowest" pivot vector (piv) (the one with rightmost pivot)
  # and subtracts itself from the next pivot (revPiv) vector such that the free variable
  # above the current pivot vector (piv).
  for (piv in nr:1){
    message("piv: ", piv)
    for (revPiv in piv:1){
      message("revPiv: ", revPiv)
      if(piv == revPiv){
        next
      }
      A[revPiv,] = A[revPiv,]-A[piv,]*A[revPiv,piv]
    }
  }
  
  # 5.) set small values to zero
  A = ifelse(abs(A) <= 1e-10, 0, A)
  
  return(A)
}

# Plotting Functions ------------------------------------------------------

#' Title
#' Plotting eigenvectors from 2x2 matrix 
#' @param A 
#' @param offset 
#' @param plotBasisVecs 
#' @param plotSpan 
#' @param plotTransBasis 
#'
#' @return
#' @export
#'
#' @examples
plot_eigenvec <- function(A,
                          offset = 1,
                          plotBasisVecs = T,
                          plotSpan = T,
                          plotTransBasis = T){
  
  # assumptions for function:
  if (all(dim(A) != 2)){
    message("A is not 2x2. Exiting...")
    return(NULL)
  }
  
  par(pty="s")
  
  maxMat = max(A) + offset
  
  plot(1, type = "n",                        
       xlab = "", ylab = "",
       xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
       main = paste("Eigenvalues = ",eigen(A)$values), cex.main = 0.75)
  grid()
  
  # basis vectors
  if (plotBasisVecs){
    arrows(0,0,0,1, length = 0.05)
    arrows(0,0,1,0, length = 0.05)
  }
  
  # span basis vectors
  arrows(0,-1*2*maxMat,0,1*2*maxMat,
         length = 0.05, col = rgb(0,0,0,0.3), code = 0, lty = 2)
  arrows(-1*2*maxMat,0,1*2*maxMat,0,
         length = 0.05, col = rgb(0,0,0,0.3), code = 0, lty = 2)
  
  # transformed basis vectors
  if (plotTransBasis){
    arrows(0,0,A[1,1],A[2,1], length = 0.05, col = rgb(0,0,0.8,0.7))
    arrows(0,0,A[1,2],A[2,2], length = 0.05, col = rgb(0,0,0.8,0.7))
  }
  
  # span transformed basis vectors
  if (plotSpan){
    arrows(-A[1,1]*maxMat,-A[2,1]*maxMat,
           A[1,1]*maxMat,A[2,1]*maxMat,
           length = 0.05, col = rgb(0,0,0.8,0.3), code = 0, lty = 2)
    arrows(-A[1,2]*maxMat,-A[2,2]*maxMat,
           A[1,2]*maxMat,A[2,2]*maxMat,
           length = 0.05, col = rgb(0,0,0.8,0.3), code = 0, lty = 2)
  }
  
  
  # eigenvectors
  eign = eigen(A)
  
  if (any(Im(eign$values) != 0)){
    message("Some eigenvalues are complex, but only real values are considered!\n")
  }
  
  # imagenary part of eigenvectors is stripped
  eigenVec_1 = Re(eign$vectors[,1])*Re(eign$values[1]) 
  eigenVec_2 = Re(eign$vectors[,2])*Re(eign$values[2])
  
  # eigenvectors plotting
  arrows(0,0,eigenVec_1[1],eigenVec_1[2], length = 0.05, col = rgb(0.8,0,0.8,0.7))
  arrows(0,0,eigenVec_2[1],eigenVec_2[2], length = 0.05, col = rgb(0.8,0,0,0.7))
  
  # span eigenvectors plotting
  if (plotSpan){
    arrows(-eigenVec_1[1]*maxMat,-eigenVec_1[2]*maxMat,
           eigenVec_1[1]*maxMat,eigenVec_1[2]*maxMat,
           length = 0.05, col = rgb(0.8,0,0.8,0.3), lty = 2) # aes
    arrows(-eigenVec_2[1]*maxMat,-eigenVec_2[2]*maxMat,
           eigenVec_2[1]*maxMat,eigenVec_2[2]*maxMat,
           length = 0.05, col = rgb(0.8,0,0,0.3), lty = 2) # aes
  }
  
  # plot origin
  points(0,0, pch = 16, cex = 0.7,)
  
  # legend
  legend("topleft", c("Basis", "Transformed Basis","Scaled Eigenvectors"),
         col = c(rgb(0,0,0,1),rgb(0,0,0.8,0.7),rgb(0.8,0,0.8,0.7)), 
         pch = c(16,16,16),
         inset=c(1,0), xpd=TRUE, horiz=F, bty="n",
         cex = 0.75)
  
  # restore par settings to default
  par(mfrow = c(1,1))
}

#' Title
#' Plot matrix transformation Ax = y
#' @param A 
#' @param v 
#' @param offset 
#' @param plotBasisVecs 
#' @param splitPlot 
#'
#' @return
#' @export
#'
#' @examples
plot_matrix_transformation <- function(A,v,
                                       offset = 1,
                                       plotBasisVecs = T,
                                       splitPlot = T){
  # assumptions for function:
  if (all(dim(A) != 2)){
    message("A is not 2x2. Exiting...")
    return(NULL)
  }
  
  par(mfrow = c(1,2),pty="s")
  
  maxMat = max(A) + offset
  
  plot(1, type = "n",                        
       xlab = "", ylab = "",
       xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
       main = "Before Transformation", cex.main = 0.75)
  grid()
  
  # basis vectors
  if (plotBasisVecs){
    arrows(0,0,0,1, length = 0.05)
    arrows(0,0,1,0, length = 0.05)
  }
  
  # span basis vectors
  arrows(0,-1*2*maxMat,0,1*2*maxMat,
         length = 0.05, col = rgb(0,0,0,0.3), code = 0, lty = 2)
  arrows(-1*2*maxMat,0,1*2*maxMat,0,
         length = 0.05, col = rgb(0,0,0,0.3), code = 0, lty = 2)
  
  # input vector
  arrows(0,0,v[1],v[2], length = 0.05, col = rgb(0.8,0,0,0.8))
  text(v[1],v[2]+round(offset/2),paste("[",v[1],v[2],"]"),
       col = rgb(0.8,0,0,0.8), cex = 0.75)
  
  # plot origin and tip of vec
  points(0,0, pch = 16, cex = 0.7,)
  points(0,0, pch = 16, cex = 0.7,)
  
  # legend
  legend("bottomright", c("Basis","Vector x (Ax=y)"),
         col = c(rgb(0,0,0.8,0.7),rgb(0.8,0,0.8,0.7)), 
         pch = c(16,16), inset=c(0,1), xpd=TRUE, horiz=T, bty="n",
         cex = 0.75)
  
  # second plot with transformation
  
  # find new boundaries 
  v_trans = A%*%v
  if (max(v_trans) > maxMat){
    maxMat = max(v_trans) + offset
  }
  
  if (splitPlot){
    # emtpy plot
    plot(1, type = "n",                        
         xlab = "", ylab = "",
         xlim = c(-maxMat, maxMat), ylim = c(-maxMat, maxMat),
         main = "After Transformation", cex.main = 0.75)
    
    # formula for getting angle between vectors
    angle <- function(v1,v2){
      acos( sum(v1*v2) / ( sqrt(sum(v1*v1)) * sqrt(sum(v2*v2)) ) )
    }
    
    # find angle
    angle1 = angle(c(1,0),A[,1])
    angle2 = -angle(c(0,1),A[,2])
    
    
    # apply grid (swipe through grid in intervals (seq) and draw abline with slope = angle)
    sapply(seq(-maxMat*10,maxMat*10,by=1), function(inter) abline(a=inter, 
                                                                  b=tan(angle1),
                                                                  lty=3,
                                                                  col="lightgray"))
    sapply(seq(-maxMat*10,maxMat*10,by=4), function(inter) abline(a=inter, 
                                                                  b=tan(angle2+pi/2),
                                                                  lty=3,
                                                                  col="lightgray"))
  }
  
  
  # transformed basis vectors
  if (plotBasisVecs){
    arrows(0,0,A[1,1],A[2,1], length = 0.05, col = rgb(0,0,0.8,0.7))
    arrows(0,0,A[1,2],A[2,2], length = 0.05, col = rgb(0,0,0.8,0.7))
  }
  
  # span transformed basis vectors
  
  arrows(-A[1,1]*maxMat,-A[2,1]*maxMat,
         A[1,1]*maxMat,A[2,1]*maxMat,
         length = 0.05, col = rgb(0,0,0.8,0.3), code = 0, lty = 2)
  arrows(-A[1,2]*maxMat,-A[2,2]*maxMat,
         A[1,2]*maxMat,A[2,2]*maxMat,
         length = 0.05, col = rgb(0,0,0.8,0.3), code = 0, lty = 2)
  
  
  
  # input vector transformed
  arrows(0,0,v_trans[1],v_trans[2], length = 0.05, col = rgb(0.5,0,0.8,0.8))
  text(v_trans[1],v_trans[2]+round(offset/2),paste("[",v_trans[1],v_trans[2],"]"),
       col = rgb(0.5,0,0.8,0.8), cex = 0.75)
  
  # legend
  legend("bottomright", c("Basis","Vector y (Ax=y)"),
         col = c(rgb(0,0,0.8,0.7),rgb(0.8,0,0.8,0.7)), 
         pch = c(16,16), inset=c(0,1), xpd=TRUE, horiz=T, bty="n",
         cex = 0.75)
  
  # restore par settings to default
  par(mfrow = c(1,1))
}

# Auxillary Functions -----------------------------------------------------

#' Title
#' Compare floats
#' @param a 
#' @param b 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
compare_floats <- function(a,b,tol=1e-06){
  return(abs(a-b) < tol)
}

#' Title
#' Swap rows
#' @param A 
#' @param old 
#' @param new 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
swap <- function(A,old,new, col = T) {
  tmp <- A[old,]
  A[old,] <- A[new,]
  A[new,] <- tmp
  return(A)
}

#' Title
#' Add rows to the bottom of a matrix (used in rref)
#' @param A 
#' @param to_append 
#'
#' @return
#' @export
#'
#' @examples
add_to_bottom <- function(A,to_append){
  out <- rbind(A[-to_append,],A[to_append,])
  return(out)
}

#' Title
#' Check if column vector below pivot is all zero (used in rref)
#' @param A 
#' @param currentCol 
#'
#' @return
#' @export
#'
#' @examples
col_is_all_zero = function(A,currentCol){
  check <- all(A[-c(1:currentCol),currentCol] == 0)
  return(check)
}

#' Title
#' Perform row operations (used in rref)
#' @param A 
#' @param idxNonzero 
#' @param currentCol 
#'
#' @return
#' @export
#'
#' @examples
gaussian_elimination <- function(A, idxNonzero, currentCol){
  for (ele in idxNonzero){
    A[ele,] = A[ele,]-A[ele,currentCol]*A[currentCol,]
  }
  return(A)
}

# TODO: RM if only used in swap_zero_vectors
#' Title
#'
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
find_zero_vectors = function(A){
  idx <- which(rowSums(sqrt(A^2)) == 0)
  return(idx)
}

#' Title
#' Used to put zero vectors at the bottom if found in matrix
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
swap_zero_vectors <- function(A){
  
  zeroVecIdx = find_zero_vectors(A)
  
  if (length(zeroVecIdx) == 0){
    return(A)
  } else {
    return(add_to_bottom(A,zeroVecIdx))
  }
}

#' Title
#'
#' @param A 
#'
#' @return
#' @export
#'
#' @examples
remove_parallel_vectors <- function(A){
  # find parallel vectors
  rmIdx = unique(lin_dep_Cautchy_Schwartz_matrix(A)$j)
  
  nc = ncol(A)
  # put zero-vector and parallel vectors at the bottom
  if (!is.null(rmIdx)){
    A[rmIdx,] <- rep(0,nc)
    A <- add_to_bottom(A,rmIdx)
    return(A)
  } else {
    return(A)
  }
}

#' Title
#'
#' @param scalar 
#'
#' @return
#' @export
#'
#' @examples
flip_sign <- function(scalar){
  if (scalar > 0){
    return(scalar)
  } else {
    return(-1*scalar)
  }
}
