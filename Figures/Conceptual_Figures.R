
source("/Users/aginigue/Library/CloudStorage/Box-Box/Redundancy_Project/Simulation/RescaleCor.R")
source("/Users/aginigue/Library/CloudStorage/Box-Box/Redundancy_Project/Simulation/00_project_functions.R")
library("groundhog")
groundhog.library("qgraph", date = '2026-08-30')


set.seed(1111)

lat.mat = cor.gen(nvar = 4, mn.cor = .7, sd = .1)

item.mat = ind.corr(matrix = lat.mat,
                    loadings = .9,
                    clone.loading = .9)
set.seed(2222)
lat.mat = cor.gen(nvar = 5, mn.cor = 0.7, sd = 0.1)
item.mat.rand = ind.corr(matrix = lat.mat,
                         loadings = .9,
                         redundancy = FALSE)

colnames(item.mat) = rownames(item.mat) = c(paste0("F", 1:5)) 
colnames(item.mat.rand) = rownames(item.mat.rand) = c(paste0("P", 1:5))

true.pcor = cor2pcor(item.mat[1:4, 1:4])
redun.pcor = cor2pcor(item.mat)
random.pcor = cor2pcor(item.mat.rand)



# Figure 1
pp = averageLayout(qgraph(redun.pcor,
            edge.labels = TRUE,
            DoNotPlot = TRUE))

node.colors = c("white", "white", "white", "gold")
colnames(redun.pcor) = c("P1", "P2", "P3", "Target", "Redun")
colnames(true.pcor) = c("P1", "P2", "P3", "Target")

node.colors.2 = c("white", "white", "white", "gold","lightblue")

# jpeg(filename = "Redundancy_Fig1.jpeg",
#      width = 12,
#      height = 8,
#      res = 500,
#      units = "in")


par(mfrow = c(1, 2))
qgraph(true.pcor,
       layout = pp[1:4,],
       edge.labels = TRUE,
       color = node.colors,
       labels = colnames(true.pcor),
       label.cex = 1.15,
       vsize = 12,
       edge.label.cex = 1.8,
       maximum = 0.6,
       esize = 25,
       theme = "colorblind")
text(-.28, 1.20, "Original Network",
     cex = 1.5, adj = 1)

qgraph(redun.pcor,
       layout = pp,
       edge.labels = TRUE,
       color = node.colors.2,
       labels = colnames(redun.pcor),
       label.cex = 1.15,
       vsize = 12,
       edge.label.cex = 1.8,
       maximum = 0.6,
       edge.label.position = 0.42,
       esize = 25,
       theme = "colorblind")
text(-0.1, 1.20, "Redundant Network",
     cex = 1.5, adj = 1)
# dev.off()




# Figure 3
pp.2 = averageLayout(qgraph(random.pcor,
            edge.labels = TRUE,
            DoNotPlot = TRUE))


colnames(random.pcor) = c("P1", "P2", "P3", "Target", "Random")



# jpeg(filename = "Random_Fig3.jpeg",
#      width = 12,
#      height = 8,
#      res = 500,
#      units = "in")


par(mfrow = c(1, 2))
qgraph(true.pcor,
       layout = pp.2[1:4,],
       edge.labels = TRUE,
       color = node.colors,
       labels = colnames(true.pcor),
       label.cex = 1.15,
       vsize = 12,
       edge.label.cex = 1.8,
       esize = 25,
       maximum = 0.48,
       cut = 0,
       theme = "colorblind")
text(-.45, 1.20, "Original Network",
     cex = 1.35, adj = 1)

qgraph(random.pcor,
       layout = pp.2,
       edge.labels = TRUE,
       color = node.colors.2,
       labels = colnames(random.pcor),
       label.cex = 1.15,
       vsize = 12,
       edge.label.cex = 1.8,
       edge.label.position = 0.25,
       esize = 25,
       maximum = 0.48,
       cut = 0,
       theme = "colorblind")
text(-0.5, 1.20, "Random Network",
     cex = 1.35, adj = 1)
# dev.off()



