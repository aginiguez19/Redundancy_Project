# I want to see what if my latent var correlations match my indicators 
# I need: cor.gen(), ind.corr(), and I think that's it 
# So let's run a for loop to see what happens, start simple and build up 




mean.lat = matrix(NA, nrow = 1000, ncol = nrow(conds))
mean.items = matrix(NA, nrow = 1000, ncol = nrow(conds))

conds = expand.grid(
  nvar = c(10,20),
  mn.cor = c(.2, .6, .8),
  c.loading = c(.7, .9)
)

for (j in 1:nrow(conds)){
  for (i in 1:1000){
    lat.mat = cor.gen(nvar = conds[j,]$nvar,
                      mn.cor =conds[j,]$mn.cor,
                      sd = .05)
    mean.lat[i, j] = sd(lat.mat[lower.tri(lat.mat)])
    items.mat = ind.corr(matrix = lat.mat, loadings = .9, clone.loading = conds[j,]$c.loading)
    mean.items[i, j] = sd(items.mat[lower.tri(items.mat)])
  }
}





# I want to see if sparsity affects the closeness metric when I include a redundant node more
# than when there is no sparsity and there is still a redundant node

library(igraph)
library(qgraph) 
library(corpcor)
n = 5
sparse.deltas.c = matrix(NA, nrow = 1000, ncol = n)
for (i in 1:1000){
# Start with generating the correlation matrix 
lat.mat = cor.gen(nvar = n, mn.cor = .6, sd = .05)

# Convert latent correlation matrix into a precision matrix 
prec.mat = solve(lat.mat)

# Induce sparsity to the precision matrix 
sparse.prec.mat = function(prec.mat, prop = .3, max.iter = 100){
  n = nrow(prec.mat) # Get number of nodes
  idx.lower = which(lower.tri(prec.mat)) # Get indexes of lower triangle
  n.zero = floor(prop * length(idx.lower)) # Number of 0s 
  
  for (i in 1:max.iter){
    prec.new = prec.mat  # Number of repititions until a PSD precision matrix is found
    idx.zero = sample(idx.lower, n.zero) # Sample from indexes that should be 0
    prec.new[idx.zero] = 0 # Change the sampled indexes to 0 
    
    prec.new[upper.tri(prec.new)] = t(prec.new)[upper.tri(prec.new)] # Make upper triangle the same
    
    vals = eigen(prec.new, symmetric = TRUE, only.values = TRUE)$values # Check eigens
    
    if (all(vals >= 1e-8)){
      return(prec.new)
    }
  }
}


# Induce sparsity to the precision matrix  
prec.mat = sparse.prec.mat(prec.mat)


# Take inverse to go back to latent covariance matrix 
latent.sparse = solve(prec.mat)


# Standardize 
cov2cor = function(matrix){
  inv.sd = diag(1/sqrt(diag(item.sparse)))
  matrix = inv.sd %*% matrix %*% inv.sd
  return(matrix)
}
# Standardized matrix of sparse item cov matrix
item.sparse = cov2cor(item.sparse)






# Use SEM formula to get model-implied correlation matrix
dimensions = dim(lat.mat)
lambda.matrix = diag(x = .9, nrow = dimensions[1], ncol = dimensions[2])

# Lambda * Psi * t(Lambda)
item.mat = lambda.matrix %*% lat.mat %*% t(lambda.matrix)

# Theta
dimensions.theta = dim(item.mat)
theta = diag((x = 1 - (.9)^2), nrow = dimensions.theta[1], ncol = dimensions.theta[2])
item.mat <- item.mat + theta


# Function to make a sparse precision matrix
sparse.prec.mat = function(prec.mat, prop = .3, max.iter = 100){
  n = nrow(prec.mat) # Get number of nodes
  idx.lower = which(lower.tri(prec.mat)) # Get indexes of lower triangle
  n.zero = floor(prop * length(idx.lower)) # Number of 0s 
  
  for (i in 1:max.iter){
    prec.new = prec.mat  # Number of repititions until a PSD precision matrix is found
    idx.zero = sample(idx.lower, n.zero) # Sample from indexes that should be 0
    prec.new[idx.zero] = 0 # Change the sampled indexes to 0 
    
    prec.new[upper.tri(prec.new)] = t(prec.new)[upper.tri(prec.new)] # Make upper triangle the same
    
    vals = eigen(prec.new, symmetric = TRUE, only.values = TRUE)$values # Check eigens
    
    if (all(vals >= 1e-8)){
      return(prec.new)
    }
  }
}

# Sparse precision matrix 
prec.mat = sparse.prec.mat(prec.mat)

# Take inverse to go back to item covariance matrix 
item.sparse = solve(prec.mat)

# Standardize 
cov2cor = function(matrix){
  inv.sd = diag(1/sqrt(diag(item.sparse)))
  matrix = inv.sd %*% matrix %*% inv.sd
  return(matrix)
}
# Standardized matrix of sparse item cov matrix
item.sparse = cov2cor(item.sparse)

sparse.redun = function(matrix, redun.cor = .81){
# Save target
target = matrix[, n]
# Change last element to be correlation between target and clone
target[n] = redun.cor

# Add redundancy by duplicating the last node
matrix.r = matrix(0, nrow = n +1, ncol = n +1)

# Take the original sparse matrix and fill nxn
matrix.r[1:n, 1:n] = matrix
matrix.r[1:n,(n+1)] = target
matrix.r[(n+1), 1:n] = target
matrix.r[(n+1),(n+1)] = 1

return(matrix.r)
}

# Correlation matrix with redundancy
item.sparse.r = sparse.redun(matrix = item.sparse)

# Convert both correlation matrices into partial correlation matrices and then igraph objects 

true.sparse = cor2pcor(item.sparse)
redun.sparse = cor2pcor(item.sparse.r)


# Convert to igraph objects
tsparse.igraph = graph_from_adjacency_matrix(true.sparse,
                                          mode = "undirected",
                                          weighted = TRUE)
redunsparse.igraph = graph_from_adjacency_matrix(redun.sparse,
                                                 mode = "undirected",
                                                 weighted = TRUE)



# Compute closeness for each igraph object 
close.tvals = closeness(tsparse.igraph,
          normalized = TRUE,
          weights = 1/abs(E(tsparse.igraph)$weight))

close.redunvals = closeness(redunsparse.igraph,
          normalized = TRUE,
          weights = 1/abs(E(redunsparse.igraph)$weight))

# Closeness deltas between sparse network 

sparse.deltas.c[i,] = close.redunvals[1:n] - close.tvals[1:n]
}

means = colMeans(sparse.deltas.c, na.rm = TRUE)






qgraph(input = true.sparse, edge.labels = TRUE,
       layout = "spring", theme = "colorblind", vsize = 5, maximum = .8)

qgraph(input = redun.sparse, edge.labels = TRUE, layout = "spring",
       theme = "colorblind", vsize = 5, maximum = .8)















