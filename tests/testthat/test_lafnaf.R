
# TODO: Clean up!!!!!

# Libs --------------------------------------------------------------------

library(MASS)

# Unit-tests --------------------------------------------------------------

# UNIT TEST FOR canonical form
message(all(round(canonical_form(A)) == A))

# UNIT TEST FOR generalized_inverse ----

A <- t(matrix(c(2,3,1,-1,
                5,8,0,1,
                1,2,-2,3), nrow = 4))

generalized_Inverse(A)

A%*%MASS::ginv(A)%*%A
A%*%generalized_Inverse(A)%*%A

# UNIT TEST DRAFTS FOR singular_value_decomposition ----
svd(A)
res = singular_value_decomposition(A)

orthogonalize(t(A))


Y = A - res$P %*% res$D %*% t(res$Q)

s_k_left = sign(t(res$P[,1])%*%Y[,1])*(res$P[,1]%*%Y[,1])^2

s_k_right = sign(res$Q[,1]%*%Y[,1])*(res$Q[,1]%*%Y[,1])^2


K = length(diag(res$D))

res$P[,1] * t(res$Q[,1])

for (k in 1:K){
  
  for (m in 1:K){
    if (m == k){
      next
    }
    
    sum_left = sum(diag(res$D)[m] * res$P[,m] %*% res$Q[,m]) 
    
  }
  
  
}  

for (j in 1:ncol(Y)){
  
  for (m in 1:K){
    if (m == j){
      next
    }
    
    Y = A - res$P[,m] %*% res$D[,m] %*% t(res$Q[,m])
  }
  
}

s_k_left = res$P[,1] %*% Y[,1]

sign_Flip <- function(A, res){
  
}


# PLot test/example

plot_Matrix_Transformation(A,c(1,3),offset = 2)
