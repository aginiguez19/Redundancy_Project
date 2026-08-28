

library(corpcor)
library(igraph)
# Create sequence of correlations
cors = seq(from = 0.01, to = 0.99, by = 0.01)
# Empty matrix for partial correlation vals
pcor.vals = matrix(data = NA, nrow = length(cors), ncol = 4)
# Index to fill each row 
index = 1 
for (i in cors){
  psi = matrix(data = i, nrow = 10, ncol =10)# latent variable correlations
  diag(psi) = 1
  lambda = matrix(data = 0, nrow = 11, ncol = 10) # loading matrix 
  diag(lambda) = .9
  lambda[11, 10] = .9
  sigma = lambda %*% psi %*% t(lambda) # item correlation matrix
  diag(sigma) = 1
  pcor.mat = cor2pcor(sigma) # partial correlations
  pcor.vals[index,] = c(pcor.mat[10, 2], pcor.mat[9, 3], pcor.mat[10, 11],
                        pcor.mat[10, 1])
  index = index + 1
}



# pcor.mat = as.data.frame(pcor.mat)
# colnames(pcor.mat)[11] = "Clone"
# pcor.mat = pcor.mat[1:10,1:10]
# the.g  = graph_from_adjacency_matrix(pcor.mat, weighted = TRUE,
#                                      mode = "undirected")
# E(the.g)$weight


# colnames(the.g)[11] = "Clone"
# qgraph::qgraph(pcor.mat, edge.labels = TRUE, diag = TRUE,
#                fade = FALSE)
# plot(the.g)
# 
# # Yes
# sum(abs(pcor.mat[2:10,1]))
# strength(the.g, loops = FALSE)
# # What the hell
# strength(the.g)[1]
# sum(abs(pcor.mat[2:10,1])) + 2
# 
# # strength
# fuck.igraph = matrix(as.matrix(the.g, "adjacency", attr = "weight"), 10, 10)
# sum(fuck.igraph[,10])
library(tidyverse)
cor.vals = rep(cors, each = 4) # Repeat cor vals for 3
colnames(pcor.vals) = c("target_random", "random_random", "target_clone", "target_random2")
pcor.tibble = as_tibble(pcor.vals)
pcor.long = pcor.tibble |> 
  pivot_longer(col = everything(),
               names_to = "Node_Pair",
               values_to = "pcors") |> 
  mutate(cors = cor.vals,
         Node_Pair = factor(x = Node_Pair,
                            levels = c("target_random",
                                       "random_random",
                                       "target_clone",
                                       "target_random2"),
                            labels = c("Target, Peripheral",
                            "Peripheral, Peripheral",
                            "Target, Clone",
                            "Target, Peripheral")))

jpeg(filename = "10,0.9.jpeg",
     width = 12,
     height = 6,
     res = 800,
     units = "in")
p = pcor.long |>
  ggplot(mapping = aes(x = cors,
                       y = pcors,
                       color = Node_Pair)) +
  geom_line()

the.g = p + labs(x = "Latent Correlations",
         y = "Partial Correlations",
         title = "Network With High Redundancy (p = 11)") +
  theme_minimal() +
  guides(color = guide_legend(title = "Node Pair")) +
  my_theme()
print(the.g)
dev.off()




# colnames(pcor.mat) = c(paste0("P", 1:9), "Target", "Clone")

# jpeg(filename = "10node.jpeg",
#      width = 12,
#      height = 6,
#      res = 800,
#      units = "in")
# qgraph(pcor.mat,
#        layout = "spring",
#        labels = colnames(pcor.mat),
#        edge.labels = TRUE,
#        theme = "colorblind",
#        vsize = 10
#        )
# dev.off()





