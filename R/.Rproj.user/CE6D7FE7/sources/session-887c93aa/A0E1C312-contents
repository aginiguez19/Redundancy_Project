
# Metrics -----------------------------------------------------------------
library(igraph)
library(kableExtra)
#ASPL
aspl <- function(qgraph_object){
  cent <- centrality(qgraph_object)
  sp <- cent$ShortestPathLengths
  aspl <- mean(sp[(upper.tri(sp, diag = FALSE))])
  return(aspl)
}


#Centrality
centralityPlot(list(Original = graph_a2, Redundant =  graph_b2,
                   Uncorrelated.Same.Function =  graph_c2,
                    Correlated.Diff.Function = graph_e2,
                    Redundant.Collapsed = graph_collapse2,
                    Uncorrelated.Collapsed = graph_collapse_b2,
                   Correlated.Collapsed = graph_collapse_b4),
               include = c("Strength", "Closeness"))

?centralityPlot
#Density

node_density <- function(qgraph_object){
  igraph <- as.igraph(qgraph_object)
  num_edges <- sum(abs(E(igraph)$weight)>0.09)
  num_nodes <- vcount(igraph)
  max_edges <- (1/2)*(num_nodes*(num_nodes-1))
  density <- num_edges/max_edges
  return(density)
}




# 9->10 Node Network ------------------------------------------------------
model.names <- c("Original",
                 "Redundant",
                 "Uncorrelated Same Function",
                 "Correlated Diff Function",
                 "Redundant Collapsed",
                 "Uncorrelated Collapsed",
                 "Correlated Collapsed")
model_list2 <- list(graph_a2, graph_b2, graph_c2, graph_e2,
                    graph_collapse2, graph_collapse_b2,
                    graph_collapse_b4)

sapply(model_list2, aspl)
sapply(model_list2,node_density)

results2 <- data.frame(
  Model.Names = model.names,
  ASPL = sapply(model_list2, aspl),
  Density = sapply(model_list2,node_density))
results2 |> 
  kable(digits = 2, 
        caption = "<center>Network Model Metrics<center>",
align = "l")|> 
  kable_classic()

centrality_auto(graph_e2)




library(qgraph)




