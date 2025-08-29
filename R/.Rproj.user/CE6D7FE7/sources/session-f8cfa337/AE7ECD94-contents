

# Tips --------------------------------------------------------------------
# These are coding examples I found helpful while working on this  project

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



#Wrapping a matrix with c() will flatten it into a vector
mat <- matrix(data = 1, nrow = 2, ncol = 2)
c(mat)

#Gives you the last element
tail(c(mat), 1)

#For loop practice
mat <- matrix(rnorm(100), ncol = 5)
par(mfrow = c(2,3))
for (i in 1:5){
  hist(x = mat[,i], main = paste("Distribution", i),
       xlab = "X Values", col = "firebrick")
}
