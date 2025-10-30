
# library(qgraph)

# Correlation Matrix Function ---------------------------------------------

# EF = effective features (correlations)
# nv = number of variables (nodes) 
# edge.probability = probability of an edge being present

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


# True and Redundant Correlation Matrix Function  ----------------------------------------------------------
# If creating a random network, specify the # of nodes after including a random
# node, example: nv = 5 if I want my true network to be 4 nodes

# If creating a redundant network, specify the # of nodes of the true network,
# example: nv = 4 if I want my true network to be 4 nodes 

# matrix = any sized matrix
# loadings = the factor loadings of peripheral and target nodes
# clone_loading = the factor loading of the clone node aka copy of the target
# default to .9
# redundancy = TRUE as default, FALSE means you are generating a random network

ind_corr <- function(matrix, loadings, clone_loading = .9, redundancy = TRUE){
  # Lambda
  dimensions <- dim(matrix)
  if (redundancy == FALSE) {
    lambda_matrix = diag(x = loadings, nrow = dimensions[1], ncol = dimensions[2])
    
    # Lambda * Psi * t(Lambda)
    random_mat <- lambda_matrix %*% matrix %*% t(lambda_matrix)

    # Theta
    dimensions_theta <- dim(random_mat)
    theta <- diag((x = 1 - (loadings)^2), nrow = dimensions_theta[1], ncol = dimensions_theta[2])
    random_mat <- random_mat + theta
    return(random_mat)
  }
  lambda_matrix <- diag(x = loadings, nrow = dimensions[1] + 1 , ncol = dimensions[2])
  last_element <- nrow(lambda_matrix) * ncol(lambda_matrix)
  lambda_matrix[last_element] <- clone_loading

  # Lambda * Psi * t(Lambda)
  redun_mat <- lambda_matrix %*% matrix %*% t(lambda_matrix)
lambda_matrix
  # Theta
  dimensions_theta <- dim(redun_mat)
  theta <- diag((x = 1 - (loadings)^2), nrow = dimensions_theta[1], ncol = dimensions_theta[2])
  theta[dimensions_theta[1],dimensions_theta[2]] <- (1-(clone_loading)^2)
  redun_mat <- redun_mat + theta
  return(redun_mat)
}


# Weighted Density Function --------------------------------------------------------
# This function takes your igraph object and whether you want weighted density
# using "expected influence" aka strength or psych strength

weighted_density = function(igrph.object, abs = TRUE){
  if (abs == FALSE){
    density = sum(E(igrph.object)$weight)/(length(igrph.object)*(length(igrph.object-1)/2))
  }
  else {
    density = sum(abs(E(igrph.object)$weight))/(length(igrph.object)*(length(igrph.object-1)/2))
  }
  
  return(density)
}


# Function Test ---------------------------------

# for (i in 1:10){
# lat_corr <- corr_gen(nv = 5, EF = .5, edge.probability = .9)
# random_corr <- ind_corr(matrix = lat_corr, loadings = .9, clone_loading = .9, redundancy = FALSE)
# random_pcor <- cor2pcor(random_corr)
# true_corr <- random_corr[1:(nv-1), 1:(nv-1)]
# true_pcor <- cor2pcor(true_corr)
# colnames(random_pcor) <- c(paste0("P",1:(nv)))
# colnames(true_pcor) <- c(paste0("P", 1:(nv-1)))
# if (i == 1){
#   g2 = qgraph(random_pcor, edge.labels = TRUE, DoNotPlot = TRUE)
#   pp = averageLayout(g2)}
# par(mfrow = c(1, 2))
# g1 <- qgraph(true_pcor, layout = pp[1:(nv-1),],
#              edge.labels = TRUE,
#              theme = "colorblind",
#              labels = colnames(true_pcor),
#              maximum = 1,
#              vsize = 9)
# g2 <- qgraph(random_pcor,layout = pp,
#        edge.labels = TRUE,
#        labels = colnames(random_pcor),
#        theme = "colorblind",
#        maximum = 1, vsize = 9)
# }









# Collapsing a Redundant Node -------------------------------------------------

# a <- matrix(c(1, 0, 0, 0, 1,
#              0, 1, 0, 0, 0,
#              0, 0, 1, 0, 0,
#               0, 0, 0, 1, 0),4,5, byrow = T) 
# c <- t(a)
# collapse_mat <- a%*%redun_mat%*%c
# collapse_model <- pcor(collapse_mat)
# graph_collapse <- qgraph(collapse_model, fade = T, edge.labels = TRUE,
#                          edge.label.cex = 1.5, layout = "circle",
#                          vsize = 10, vTrans = 260,
#                          groups = groups2,
#                          title = "Collapsed P + 1 Redundant Model",
#                          theme = "colorblind")





