cor.gen = function(nvar = NULL,
                   bg.cor = 0.30,
                   mn.cor = 0.50,
                   prop.cor = 1,
                   iter.lim = 100,
                   sd = 0.1,
                   tol.eig = 1e-1,
                   mean.tol = 0.005,
                   clip = 0.99){
  


  flag = 0
  iter = 0

  
  n_lower = (nvar^2 - nvar) / 2 # Unique off-diagonal elements 
  n.big = floor(n_lower * prop.cor) # floor will never be greater than itself, 10.5 = 10
  # take # of unique off-diag elements and multiply proportion of correlations for n.big so this is how many will be larger correlations 
  while(flag < 1){
    iter = iter + 1

    temp = matrix(rnorm(nvar^2, mean = bg.cor, sd = sd), nvar, nvar) # Initial correlation matrix 
    diag(temp) = 1 # Set diagonals to 1 to be correlations 
    
    idx.lower = which(lower.tri(temp)) # R column-major indexing 
    
    if (n.big > 0) {
      idx.big = sample(idx.lower, n.big)
      temp[idx.big] = rnorm(n.big, mean = mn.cor, sd = sd) # Replace whatever proportion of elements should have mn.cor  size correlations
    } else {
      idx.big = integer(0)
    }
    
    temp[lower.tri(temp)] = pmax(pmin(temp[lower.tri(temp)], clip), -clip)
    # any values less than or greater than clip will be replaced by clip, making the correlations bounded between -1 and 1
    temp[upper.tri(temp)] = t(temp)[upper.tri(temp)]
    # Replace lower elements with upper
    diag(temp) = 1
    
    # Eigen check
    E = eigen(temp, symmetric = TRUE) # Provides eigenvalues and vectors, eigen decomposition 
    # t(E$vectors) %*% E$vectors will give the identity matrix, all eigenvalues must be >= 0
    # E$vectors %*% diag(E$values, nrow = 5, ncol = 5) %*% solve(E$vectors) = temp (although symmetric)
    vals = E$values # Eignevalues
    vals[vals < tol.eig] = tol.eig # Replace any with a really small positive value that are less than 0
    temp.psd = E$vectors %*% (diag(vals, nvar, nvar)) %*% t(E$vectors) # eigenvectors are orthonormal so that its inverse is equal to its tranpose
    # Rebuild the matrix that is psd  
    d = sqrt(diag(temp.psd)) # Ensure it is correlation matrix
    temp_cor = temp.psd / outer(d, d)
    diag(temp_cor) = 1
    
    if (length(idx.big) > 0) {
      big.mean = mean(temp_cor[idx.big]) # Average correlation size of larger correlations 
    } else {
      big.mean = NA_real_
    }
    setdiff(idx.lower, idx.big) # Returns elements present in first object but not in 2nd
    idx.bg = setdiff(idx.lower, idx.big)
    if (length(idx.bg) > 0) {
      bg.mean = mean(temp_cor[idx.bg])
    } else {
      bg.mean = NA_real_
    }
    # Write what iteration and the average correlations of either baseline or larger
    cat("\r", paste0("Iter ", iter,
                     " | big mean=", ifelse(is.na(big.mean), "NA", round(big.mean, 3)),
                     " | bg mean=", ifelse(is.na(bg.mean), "NA", round(bg.mean, 3))), sep = "")
    flush.console()
    # Tolerance, if less than whatever mean.tol is then move on(how off we are willing to be from set correlation)
    ok_big = if (length(idx.big) > 0) abs(big.mean - mn.cor) <= mean.tol else TRUE
    ok_bg  = if (length(idx.bg) > 0)  abs(bg.mean - bg.cor) <= mean.tol else TRUE
    ok = ok_big && ok_bg
    # If TRUE send loop and return PSD matrix 
    if (ok) {
      flag = 1
      cat("\n")
      return(temp_cor)
    }
    
    if (iter >= iter.lim) {
      flag = 1
      cat("\nReached Iteration Limit. Returning last PSD correlation matrix.\n")
      return(temp_cor)
    }
  }
  
  cat("\n")
  return(NULL)
}
# 
# x = cor.gen(nvar = 3, 
#             bg.cor = 0.30, 
#             mn.cor = 0.50, 
#             prop.cor = 1, 
#             iter.lim = 50000, 
#             sd = 0.5, 
#             tol.eig = 1e-8, 
#             mean.tol = 0.04, 
#             clip = 0.99)
# 
# 
# 
# min(Re(eigen(temp)$values)) > 0 & isSymmetric(temp)
# hist(temp[temp!=1])
# mean(temp[temp!=1]); sd(temp[temp!=1])
