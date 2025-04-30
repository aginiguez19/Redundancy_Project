


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
    if(min(eigen(diag_mat, only.values = TRUE)$values)>=0){
      flag = 1
    }
  }
  return(diag_mat)
}



#Specify EF to be negative or positive?
#count number of negative edges present quantifying difference in edge weights
corr_gen(nv = 3, EF = 0, edge.probability = .5)


# Changing code for line "edge vectors" to one step -----------------------

#Original
edge_vector <- rbinom(n = n_elements, size = 1, prob = edge.probability)
edge_vector <- replace(edge_vector, edge_vector == 1, EF)
#Update
edge_vector <- ifelse(rbinom(n = n_elements, size = 1,
                             prob = edge.probability) == 1,
       sample(round(x = seq(-1,1, by = .05), 2), size = n_elements), 0)



# Changing computational efficiency for PSD check -------------------------
#Original
if(is.positive.semi.definite(diag_mat) == TRUE)
  
#Update
if(min(eigen(diag_mat, only.values = TRUE)$values) >= 0)


# Changing sampling method  -----------------------------------------------

#Orginal
sample(round(x = seq(-EF,EF, by = .05), 2), size = n_elements)

#Update       
round(rnorm(n_elements, mean = .5, sd = .2), 4)

# Tasks -------------------------------------------------------------------
#Check if sum of lower triangle is all 0 


# Helpful Tips ------------------------------------------------------------
rbinom(n = 3, size = 1, prob = .5)
#n is the # of observations (think samples) 
#size is the # of trials (think how many trials in each sample)
#p is probability of success

sample(round(x = seq(-1,1, by = .05), 2), size = 3)
#x is a vector of elements to choose
#size gives the number of items to choose

ifelse(rbinom(n = 3, size = 1, prob = .5) == 1,
       sample(round(x = seq(-1,1, by = .05), 2), size = 3), 0)
#test is a logical argument
#Second argument will replace said value if logical statement is TRUE
#Third argument will be resulting output if FALSE

rnorm(n = 20, mean = 0.4, sd = .2)
#n is the # of elements to sample 
#The mean if values came from a normal distribution
#The sd if values came from a normal distribution



