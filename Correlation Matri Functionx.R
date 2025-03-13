


# Correlation Matrix Function ---------------------------------------------
library(matrixcalc)



EF <- .5
nv <- 4
edge.probability <- 0.5


flag = 0
corr.gen = function(nv, EF, edge.probability){
  while(flag < 1){
    diag_mat <- diag(1,nv)
    off_diag_elements <- diag_mat[lower.tri(diag_mat)]
    n_elements <- length(off_diag_elements)
    edge_vector <- rbinom(n = n_elements, size = 1, prob = edge.probability)
    edge_vector <- replace(edge_vector, edge_vector == 1, EF)
    diag_mat[lower.tri(diag_mat)] <- edge_vector
    diag_mat[upper.tri(diag_mat)] <- t(diag_mat)[upper.tri(diag_mat)]
    return(diag_mat)
    if(is.positive.semi.definite(diag_mat) == T){
      flag = 1
    }
  }
}



# Check if the matrix you made is PSD; if so, set flag to 1 otherwise, flag stays 0 and keeps going






flag = 0
while(flag < 1){
  sampled.value = rnorm(1, 0, 1)
  print(sampled.value)
  if(sampled.value > 3){
    flag = 1
  }
}



 






