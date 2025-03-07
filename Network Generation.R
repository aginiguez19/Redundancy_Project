library(clusterGeneration)
library(matrixcalc)
library(qgraph)


#clusterGeneration Matrices
set.seed(2006)

#Fucntion for converting matrix to partial correlation matrix
pcor <- function(matrix){
  precision <- solve(matrix)
  pcor_matrix <- wi2net(precision)
  return(pcor_matrix)
}


mat <- genPositiveDefMat(4, covMethod = 'eigen',
                         eigenvalue = c(1, 0.5, .25, .05))

#PxP Matrix
pxp_mat<- as.matrix(mat$Sigma)

model_a <- pcor(pxp_mat)
graph_a <- qgraph(model_a, edge.labels = TRUE,
                  edge.width = 2, layout = "circle",
                  asize = 5, vTrans = 260, theme = "colorblind",
                  title = "PxP Model")





#P+1xP+1 Matrix
pxp_mat
redun_mat <- cbind(pxp_mat, c(.62 + rnorm(1, 0, 0.001),
                              0.183 + rnorm(1, 0, 0.001),
                              0.0827 + rnorm(1, 0, 0.001),
                              0.272 + rnorm(1, 0, 0.01)))

copy <- redun_mat[,5]
redun_mat <- rbind(redun_mat, c(copy, 0.64360676))
redun_mat
model_b <- pcor(redun_mat)
model_b
graph_b <- qgraph(model_b, edge.labels = TRUE, fade = T,
                  edge.width = 2, layout = "circle",
                  asize = 5, vTrans = 260,
                  title = "P+1 Redundant Model",
                  groups = groups,
                  legend = TRUE,
                  theme = "colorblind")

groups <- list(Original = c(2,3,4), Redundant = c(1,5))




legend(x = "topright", inset = c(0, 0), bty = "n", legend = c("Original Nodes",
                                                            "Redundant "),  
       col = c(rep("#E69F00",4),rep("#56B4E9",1)), y.intersp = 1,
       pch = 19, cex = 1.5)


#P+1XP+1 uncorrelated matrix
pxp_mat
uncor_mat <- cbind(pxp_mat, c(0.415 + rnorm(1, 0, 0.001),
                              0.183 + rnorm(1, 0, 0.001),
                              0.0827 + rnorm(1, 0, 0.001),
                              0.272 + rnorm(1, 0, 0.01)))

copy_2 <- uncor_mat[,5]
uncor_mat <- rbind(uncor_mat, c(copy_2, 0.644))
model_c <- pcor(uncor_mat)
model_c
graph_c <- qgraph(model_c, edge.labels = TRUE,
                  edge.label.cex = 1.5, layout = "circle",
                  vsize = 10, vTrans = 260,
                  title = "P+1 Uncorrelated Model",
                  theme = "colorblind")


#P+1XP+1 Random Matrix
pxp_mat
random_matrix <- as.matrix(genPositiveDefMat(5)$Sigma)
#This is incorrect
random_matrix
copy_3 <- random_matrix[1:4,5]
copy_3
copy_4 <- random_matrix[5,]
rand_mat <- cbind(pxp_mat, copy_3) #Could write an ifesle tree here
rand_mat <- rbind(rand_mat, copy_4)
model_d <- pcor(rand_mat)
graph_d <- qgraph(model_d, edge.labels = TRUE, labels = 1:5,
                  edge.label.cex = 1.5, layout = "circle",
                  vsize = 10, vTrans = 260,
                  title = "P+1 Random Model")

#Collapse redundant Matrix
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
                         title = "Collapsed Redundant Model")


#Collapse uncorrelated redundant matrix
collapse_mat_uncor <- a%*%uncor_mat%*%c
collapse_uncor_model <- pcor(collapse_mat_uncor)
graph_collapse_b <- qgraph(collapse_uncor_model, fade = T, edge.labels = T,
                           edge.label.cex = 1.5, layout = "circle",
                           vsize = 10, vTrans = 260,
                           title = "Collapsed Uncorrelated Model")



#Plot
par(mfrow = c(2,3))
plot(graph_a)
plot(graph_b)
plot(graph_c)
plot(graph_d)
plot(graph_collapse)
plot(graph_collapse_b)


# 9 Node Network ----------------------------------------------------------



#Increase nodes
set.seed(2011)
values <- runif(9, min = 1e-12, max = 0.8)
mat2 <- genPositiveDefMat(9, covMethod = 'eigen',
                          eigenvalue = values)

#PxP2Matrix
pxp_mat2<- as.matrix(mat2$Sigma)

model_a2 <- pcor(pxp_mat2)
graph_a2 <- qgraph(model_a2, edge.labels = TRUE, rescale = T,
                  edge.label.cex = 1,  layout = "circle",
                  vsize = 5, vTrans = 260, theme = "colorblind",
                  title = "PxP Model 2")


#P+1xP+1 Matrix v2 general redundancy
groups <- list(Original = c(1:9), Redundant = 10)
redun_mat2 <- cbind(pxp_mat2, c(.49 + rnorm(1, 0, 0.001),
                                pxp_mat2[2:9,1] + rnorm(1, 0, 0.001)))
copy_v2 <- redun_mat2[,10]
redun_mat2 <- rbind(redun_mat2, c(copy_v2, pxp_mat2[1,1]))
model_b2 <- pcor(redun_mat2)
graph_b2 <- qgraph(model_b2, edge.labels = TRUE, fade = T,
                   edge.label.cex = 1, layout = "circle", theme = "colorblind",
                   vsize = 5, vTrans = 260,
                   title = "P+1 Redundant Model 2")



#Collapsed Redundant 
a_2 <- matrix(0, 9, 10)
a_2 <- diag(10)
a_2 <- a_2[-10,] 
a_2[1,10] <- 1

c_2 <- t(a_2)

collapse_mat_2 <- a_2%*%redun_mat2%*%c_2
collapse_model2 <- pcor(collapse_mat_2)
graph_collapse2 <- qgraph(collapse_model2, fade = T, edge.labels = TRUE,
                         edge.label.cex = 1, layout = "circle",
                         vsize = 5, vTrans = 260, theme = "colorblind",
                         title = "Collapsed Redundant Model 2")

#Uncorrelated same function
uncor_mat2 <- cbind(pxp_mat2, c(0.2 + rnorm(1, 0, 0.001),
                                pxp_mat2[2:9,1] + rnorm(1, 0, 0.001)))

copy_uncor2 <- uncor_mat2[,10]
uncor_mat2 <- rbind(uncor_mat2, c(copy_uncor2, pxp_mat2[1,1]))
model_c2 <- pcor(uncor_mat2)
graph_c2 <- qgraph(model_c2, edge.labels = TRUE,
                  edge.label.cex = 1, layout = "circle", theme = "colorblind",
                  vsize = 5, vTrans = 260,
                  title = "P+1 Uncorrelated Model 2")


#Collapsed uncorrelated
collapse_mat_uncor2 <- a_2%*%uncor_mat2%*%c_2
collapse_uncor_model2 <- pcor(collapse_mat_uncor2)
graph_collapse_b2 <- qgraph(collapse_uncor_model2, fade = T, edge.labels = T,
                           edge.label.cex = 1.5, layout = "circle",
                           theme = "colorblind",
                           vsize = 5, vTrans = 260,
                           title = "Collapsed Uncorrelated Model 2")


#Strong correlated different function
corr_mat2 <- cbind(pxp_mat2, c(.49 + rnorm(1, 0, 0.001),
                                pxp_mat2[2:9,1] - rnorm(1, 0, 0.01)))
copy_corr2 <- corr_mat2[,10]
corr_mat2 <- rbind(corr_mat2, c(copy_corr2, pxp_mat2[1,1]))

corr_mat2
model_e2 <- pcor(corr_mat2)
graph_e2 <- qgraph(model_e2, edge.labels = TRUE, fade = T, 
                   edge.label.cex = 1, layout = "circle",
                   vsize = 5, vTrans = 260, theme = "colorblind",
                   groups = groups,
                   title = "P+1 Correlated different function Model")

jpeg("P+1 Correlated Different Function Model.jpg", width=24,
     height=12, res=800, units="in")
graph_e2 <- qgraph(model_e2, edge.labels = TRUE, fade = T, 
                   edge.label.cex = 1, layout = "circle",
                   vsize = 4, vTrans = 260, theme = "colorblind",
                   groups = groups, legend.cex = 2)
mtext("P+1 Correlated Different Function Model",
      line = 1, cex = 2.5, font = 1)
dev.off() 




model_e2
plot(graph_e2)
#Collapsed correlated 
collapse_mat_corr2 <- a_2%*%corr_mat2%*%c_2
collapse_corr_model2 <- pcor(collapse_mat_corr2)
graph_collapse_b4 <- qgraph(collapse_corr_model2, fade = T, edge.labels = T,
                            edge.label.cex = 1.5, layout = "circle",
                            vsize = 5, vTrans = 260, theme = "colorblind",
                            title = "Collapsed Correlated Model 2")
#This need to be changed eventually



# Mijke's code ------------------------------------------------------------


#create a 'group' vector that says which variables belong to which construct:
groups <- c(rep("Commitment to School", 7), rep("Self-Esteem", 10)) 

#create a vector of variable labels for the figure
labels <- c("SC1","SC2","SC3","SC4","SC5", "SC6", "SC7",
            "SE1","SE2","SE3","SE4","SE5","SE6","SE7", "SE8", 
            "SE9", "SE10")

diag(beta)<- 0
MaxEdge <- max(beta)
#plot CLPN results for each wave: 
jpeg("CLPN_Constrained_hybrid.jpg", width=24, height=12, res=800, units="in")  #(uncomment this code to save plot as jpeg)

par(mar = rep(0, 4))
layout(mat = matrix(c(1, 2), 1, 2), widths = c(1, 1)) #to plot multiple graphs in one figure

qgraph(t(beta), groups = groups, labels = labels, legend = FALSE,
       color = c("#E69F00", "#56B4E9"), maximum = MaxEdge,
       asize = 5, edge.width = 2, theme = "colorblind") 

#legend
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = mylegend,  
       col = c(rep("#E69F00",7),rep("#56B4E9",10)), y.intersp = 1,
       pch = 19, cex = 1.8) #add legend to identify variables
dev.off() #(uncomment this code to save plot as jpeg)

