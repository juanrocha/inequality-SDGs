library(tidyverse)

load("data/ordination_results.Rda")

m <- "ward.D2" # minimize the total within-cluster variance

clust_num <- NbClust::NbClust( 
    data = (pca1$x[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all')

## Stability & Internal validation
stab <- clValid::clValid(
    obj = pca1$x[,1:10] ,
    nClust = 2:10,
    clMethods = c(
        "hierarchical", "kmeans",  "som", 
        "model", "diana", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan",
    method = "ward.D2",
    verbose = FALSE
) ## Hierarchical is the optimal method

clValid::optimalScores(stab)
