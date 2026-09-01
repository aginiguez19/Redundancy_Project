# The Effect of a Redundant Node on Centrality Estimates
This repository contains the `.R` files to reproduce our simulation results and figures for Iñiguez, Rhemtulla, and Park (2026).

## Contents

- [Simulation](Simulation/) contains the simulation script, helper functions, and results saved as `.rds`. Additionally, `RescaleCor.R` holds the `cor.gen` function used to generate the latent correlation matrices, **$\Psi$**.

- [Figures](Figures/) holds code to turn the output produced by `01_sim_redun_networks.Rmd` into all the plots from our results section. All conceptual figures are also available here. Data are simulated, and no empirical data are used.

-  [Supplementary_Material](Supplementary_Material/) contains an additional analysis not included in the final paper. Specifically, we examined how the partial correlation between the redundant node pair changed as the average correlation was increased from 0.01 to 0.99 by increments of 0.01. An excerpt containing our interpretation of the outcome is available here.

## Script Execution

1. Run code in the [01_sim_redun_networks.Rmd](Simulation/01_sim_redun_networks.Rmd) file to reproduce our simulation. All conditions (i.e., $p$, $\rho$, redundancy level, and additional node type) are fully crossed for 500 iterations. Please edit `saveRDS()` to match your local directory before running the code.  

2. Load output from your local directory and run [02_plots.Rmd](Figures/02_plots.Rmd) to produce centrality measure figures. Example network figures from the manuscript can be reproduced using [Conceptual_Figures.R](Figures/Conceptual_Figures.R)  

Each iteration initially samples from a normal distribution with an average correlation (i.e., 0.1, 0.3, 0.5, 0.7, 0.9) and a standard deviation of 0.1, a latent correlation matrix that is used to generate the random network. Then, the same latent correlation matrix is subsetted to generate the redundant network. 

## Runtime

The simulation takes approximately 2.54 minutes on a 2026 MacBook Air M5 16 GB.

## Session Info

R version 4.6.1 (2026-06-24)
Platform: aarch64-apple-darwin23
Running under: macOS Tahoe 26.6.2, RStudio 2026.8.2.200

attached base packages:

[1] stats     graphics  grDevices utils     datasets  methods  
[7] base     

other attached packages:

[1] xfun_0.60     igraph_2.3.3  qgraph_1.10.1

loaded via a namespace (and not attached):
 
 [1] generics_0.1.4      gtools_3.9.5        jpeg_0.1-11        
 [4] stringi_1.8.9       lattice_0.23-1      digest_0.6.39      
 [7] magrittr_2.0.5      evaluate_1.0.5      grid_4.6.1         
[10] RColorBrewer_1.1-3  fastmap_1.2.0       plyr_1.8.9         
[13] Matrix_1.7-6        nnet_7.3-21         backports_1.5.1    
[16] Formula_1.2-6       gridExtra_2.3.1     scales_1.4.0       
[19] pbapply_1.7-4       pbivnorm_0.6.0      abind_1.4-8        
[22] mnormt_2.1.2        cli_3.6.6           rlang_1.3.0        
[25] Hmisc_5.2-6         base64enc_0.1-6     yaml_2.3.12        
[28] otel_0.2.0          parallel_4.6.1      tools_4.6.1        
[31] reshape2_1.4.5      checkmate_2.3.4     htmlTable_2.5.0    
[34] dplyr_1.2.1         colorspace_2.1-3    corpcor_1.6.10     
[37] ggplot2_4.0.3       vctrs_0.7.3         fdrtool_1.2.18     
[40] R6_2.6.1            png_0.1-9           rpart_4.1.27       
[43] stats4_4.6.1        lifecycle_1.0.5     stringr_1.6.0      
[46] htmlwidgets_1.6.4   psych_2.6.5         foreign_0.8-91     
[49] cluster_2.1.8.3     glasso_1.11         pkgconfig_2.0.3    
[52] pillar_1.11.1       gtable_0.3.6        glue_1.8.1         
[55] data.table_1.18.6.1 Rcpp_1.1.2          xfun_0.60          
[58] tibble_3.3.1        tidyselect_1.2.1    rstudioapi_0.19.0  
[61] knitr_1.51          farver_2.1.2        nlme_3.1-170       
[64] htmltools_0.5.9     lavaan_0.7-2        rmarkdown_2.31     
[67] compiler_4.6.1      quadprog_1.5-8      S7_0.2.2 

## Citation

## License

This project is licensed under the terms of the [`MIT`](LICENSE.md) license.

## Contact
Please contact Abraham Iñiguez (aginiguez@ucdavis.edu) should any questions arise. 
