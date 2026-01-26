cor.gen = function(nvar = NULL,
                   bg.cor = 0.30,
                   mn.cor = 0.50,
                   prop.cor = 0.50,
                   iter.lim = 100,
                   sd = 0.25,
                   tol.eig = 1e-8,
                   mean.tol = 0.02,
                   clip = 0.99){
  
  flag = 0
  iter = 0
  
  n_lower = (nvar^2 - nvar) / 2
  n.big = floor(n_lower * prop.cor)
  
  while(flag < 1){
    iter = iter + 1
    
    temp = matrix(rnorm(nvar^2, mean = bg.cor, sd = sd), nvar, nvar)
    diag(temp) = 1
    
    idx.lower = which(lower.tri(temp))
    
    if (n.big > 0) {
      idx.big = sample(idx.lower, n.big)
      temp[idx.big] = rnorm(n.big, mean = mn.cor, sd = sd)
    } else {
      idx.big = integer(0)
    }
    
    temp[lower.tri(temp)] = pmax(pmin(temp[lower.tri(temp)], clip), -clip)
    
    temp[upper.tri(temp)] = t(temp)[upper.tri(temp)]
    diag(temp) = 1
    
    E = eigen(temp, symmetric = TRUE)
    vals = E$values
    vals[vals < tol.eig] = tol.eig
    temp.psd = E$vectors %*% (diag(vals, nvar, nvar)) %*% t(E$vectors)
    
    d = sqrt(diag(temp.psd))
    temp_cor = temp.psd / outer(d, d)
    diag(temp_cor) = 1
    
    if (length(idx.big) > 0) {
      big.mean = mean(temp_cor[idx.big])
    } else {
      big.mean = NA_real_
    }
    
    idx.bg = setdiff(idx.lower, idx.big)
    if (length(idx.bg) > 0) {
      bg.mean = mean(temp_cor[idx.bg])
    } else {
      bg.mean = NA_real_
    }
    
    cat("\r", paste0("Iter ", iter,
                     " | big mean=", ifelse(is.na(big.mean), "NA", round(big.mean, 3)),
                     " | bg mean=", ifelse(is.na(bg.mean), "NA", round(bg.mean, 3))), sep = "")
    flush.console()
    
    ok_big = if (length(idx.big) > 0) abs(big.mean - mn.cor) <= mean.tol else TRUE
    ok_bg  = if (length(idx.bg) > 0)  abs(bg.mean - bg.cor) <= mean.tol else TRUE
    ok = ok_big && ok_bg
    
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

x = cor.gen(nvar = 20, 
            bg.cor = 0.30, 
            mn.cor = 0.50, 
            prop.cor = 1, 
            iter.lim = 50000, 
            sd = 0.25, 
            tol.eig = 1e-8, 
            mean.tol = 0.04, 
            clip = 0.99)
x
min(Re(eigen(x)$values)) > 0 & isSymmetric(x)
hist(x[x!=1])
mean(x[x!=1]); sd(x[x!=1])
