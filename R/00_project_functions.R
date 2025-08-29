


# Correlation Matrix Function ---------------------------------------------

#EF = effective features
#nv = number of variables 
#edge.probability = probability of an edge

corr_gen = function(nv, EF, edge.probability){
  flag = 0
  while(flag < 1){
    diag_mat <- diag(1,nv)
    off_diag_elements <- diag_mat[lower.tri(diag_mat)]
    n_elements <- length(off_diag_elements)
    edge_vector <- ifelse(rbinom(n = n_elements, size = 1,
                                 prob = edge.probability) == 1,
                          round(rnorm(n_elements, mean = EF, sd = .05), 4), 0)
    diag_mat[lower.tri(diag_mat)] <- edge_vector
    diag_mat[upper.tri(diag_mat)] <- t(diag_mat)[upper.tri(diag_mat)]
    if(min(eigen(diag_mat, only.values = TRUE)$values)>=0 &
       sum(diag_mat[lower.tri(diag_mat)]) != 0){
      flag = 1
    }
  }
  return(diag_mat)
}


# Correlation to Partial Correlation Function -----------------------------
# Only needs a matrix of any size as input
cor2pcor <- function(matrix){
  precision_mat <- solve(matrix)
  pcor_mat <- wi2net(precision_mat)
  pcor_mat <- as.matrix(pcor_mat)
  return(pcor_mat)
}



# Correlation of Indicators + Redundant Indicator Function  ----------------------------------------------------------
ind_corr <- function(matrix, loading, scalar = 1){
  # Lambda
  dimensions <- dim(matrix)
  lambda_matrix <- diag(x = loading, nrow = dimensions[1] + 1 , ncol = dimensions[2])
  last_element <- nrow(lambda_matrix) * ncol(lambda_matrix)
  lambda_matrix[last_element] <- loading
  
  # Lambda * Psi * t(Lambda)
  redun_mat <- lambda_matrix %*% matrix %*% t(lambda_matrix)
  
  # Theta
  dimensions_theta <- dim(redun_mat)
  theta <- diag((x = 1 - (loading)^2), nrow = dimensions_theta[1], ncol = dimensions_theta[2])
  redun_mat <- redun_mat + theta
  redun_mat[1:nv,nv+1] <- scalar*redun_mat[1:nv,nv+1]
  return(redun_mat)
}






