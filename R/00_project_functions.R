


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


ind_corr <- function(matrix, loadings, clone_loading, scalar = 1){
  # Lambda
  loadings <- .9
  clone_loading <- 0
  lat_corr <- corr_gen(nv = 4, EF = .5, edge.probability = .9)
  matrix <- lat_corr

  dimensions <- dim(matrix)
  lambda_matrix <- diag(x = loadings, nrow = dimensions[1] + 1 , ncol = dimensions[2])
  last_element <- nrow(lambda_matrix) * ncol(lambda_matrix)
  lambda_matrix[last_element] <- clone_loading

  # Lambda * Psi * t(Lambda)
  redun_mat <- lambda_matrix %*% matrix %*% t(lambda_matrix)
  if (redun_mat[dimensions[1],dimensions[2]] == 0) {
    redun_mat[5,1:3] <- redun_mat[4, 1:3]
    
  }
  # Theta
  dimensions_theta <- dim(redun_mat)
  theta <- diag((x = 1 - (loadings)^2), nrow = dimensions_theta[1], ncol = dimensions_theta[2])
  theta[dimensions_theta[1],dimensions_theta[2]] <- (1-(clone_loading)^2)
  redun_mat <- redun_mat + theta
  redun_mat[1:nv,nv+1] <- scalar*redun_mat[1:nv,nv+1]
  return(redun_mat)
}


# Correlation of Indicators Function Test ---------------------------------
nv <- 4
for (i in 1:10){
lat_corr <- corr_gen(nv = 4, EF = .5, edge.probability = .9)
redun_corr <- ind_corr(matrix = lat_corr, loadings = .6, clone_loading = .6, scalar = -1)
redun_pcor <- cor2pcor(redun_corr)
true_corr <- redun_corr[1:nv, 1:nv]
true_pcor <- cor2pcor(true_corr)
colnames(redun_pcor) <- c(paste0("P",1:(nv-1)),"Target", "Clone")
colnames(true_pcor) <- c(paste0("P", 1:(nv-1)), "Target")
if (i == 1){
  g2 = qgraph(redun_pcor, edge.labels = TRUE, DoNotPlot = TRUE)
  pp = averageLayout(g2)}
par(mfrow = c(1, 2))
g1 <- qgraph(true_pcor, layout = pp[1:nv,],
             edge.labels = TRUE,
             theme = "colorblind",
             labels = colnames(true_pcor),
             maximum = 1,
             vsize = 11)
g2 <- qgraph(redun_pcor,layout = pp,
       edge.labels = TRUE,
       labels = colnames(redun_pcor),
       theme = "colorblind",
       maximum = 1, vsize = 11)
}











# Collapse Redundant Node -------------------------------------------------
a <- matrix(c(1, 0, 0, 0, 1,
              0, 1, 0, 0, 0,
              0, 0, 1, 0, 0,
              0, 0, 0, 1, 0),4,5, byrow = T)
c <- t(a)
collapse_mat <- a%*%redun_mat%*%c
collapse_model <- pcor(collapse_mat)
graph_collapse <- qgraph(collapse_model, fade = T, edge.labels = TRUE,
                         edge.label.cex = 1.5, layout = "circle",
                         vsize = 10, vTrans = 260,
                         groups = groups2,
                         title = "Collapsed P + 1 Redundant Model",
                         theme = "colorblind")






