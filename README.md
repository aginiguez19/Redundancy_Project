# The Effect of a Redundant Node on Centrality Estimates
This repository contains the `.R` files to reproduce our simulation results and figures.

## Simulation

- [`Simulation`](Simulation/) contains the simulation script, helper functions, and results saved as `.rds`. Additionally, `RescaleCor.R` holds the `cor.gen` function used to generate the latent correlation matrices, **$\Psi$**.

- [`Figures`](Figures/) turns the output produced by `01_sim_redun_networks.Rmd` into all the plots from our results section. All conceptual figures are also available here. 


## Supplementary Material
- [`Supplementary_Material`](Supplementary_Material/) contains an additional analysis not included in the final paper. Specifically, we examined how the partial correlation between the redundant node pair changed as the average correlation was increased from 0.1 to 0.9 by increments of 0.01. An excerpt containing our interpretation of the outcome is available here.
- 
## Session Info

