

library(corpcor)
library(igraph)
library(tidyverse)

# Create sequence of correlations
cors = seq(from = 0.01, to = 0.99, by = 0.01)
# Empty matrix for partial correlation vals
pcor.vals = matrix(data = NA, nrow = length(cors), ncol = 4)
# Index to fill each row 
index = 1 
for (i in cors){
  psi = matrix(data = i, nrow = 20, ncol = 20)# latent variable correlations
  diag(psi) = 1
  lambda = matrix(data = 0, nrow = 21, ncol = 20) # loading matrix 
  diag(lambda) = .9
  lambda[21, 20] = .9
  sigma = lambda %*% psi %*% t(lambda) # item correlation matrix
  diag(sigma) = 1
  pcor.mat = cor2pcor(sigma) # partial correlations
  pcor.vals[index,] = c(pcor.mat[20, 2], pcor.mat[9, 3], pcor.mat[20, 21],
                        pcor.mat[20, 1])
  index = index + 1
}



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
                            "Target, Redundant",
                            "Target, Peripheral")))

jpeg(filename = "20,0.9.jpeg",
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
         title = "Network With High Redundancy (p = 21)") +
  theme_minimal() +
  guides(color = guide_legend(title = "Node Pair"))
print(the.g)
dev.off()










