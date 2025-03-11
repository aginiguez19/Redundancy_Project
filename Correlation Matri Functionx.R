


# Correlation Matrix Function ---------------------------------------------




library(matrixcalc)

I <- diag(1,3)
eigen(I)
matrix <- matrix(1, 3,3)
eigen(matrix)$values >= 0
i12s.positive.semi.definite()

EF <- .5
nv <- 4
diag_mat <- diag(1, nv)
off_diag_elements <- diag_mat[lower.tri(diag_mat)]
n_elements <- length(off_diag_elements)
n_elements
edge_vector <- rbinom(n_elements, 1, 0.5)
edge_vector <- replace(edge_vector, edge_vector == 1, EF)
edge_vector


corr.gen = function(nv, EF = 0.50, edge.prob = 0.50){
  # Define a flag variable to stop your loop
  while(flag < 1){
    diag_mat <- diag(1,nv)
    off_diag_elements <- diag_mat[lower.tri(diag_mat)]
    n_elements <- length(off_diag_elements)
    edge_vector <- rbinom(n = n_elements, size = 1, p = edge.prob)
    edge_vector <- replace(edge_vector, edge_vector == 1, EF)
    diag_mat[lower.tri(diag_mat)] <- edge_vector
    
    
    
    
# Populate the lower.triangular and reflect that to the upper.triangular to make a symmetric correlation matrix
# Check if the matrix you made is PSD; if so, set flag to 1 otherwise, flag stays 0 and keeps going
  }
  return(correlation.matrix)
}

flag = 0
while(flag < 1){
  sampled.value = rnorm(1, 0, 1)
  print(sampled.value)
  if(sampled.value > 3){
    flag = 1
  }
}


diag_mat

 






