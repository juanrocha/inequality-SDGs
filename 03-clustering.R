library(tidyverse)
library(NbClust)
library(clValid)
library(mclust) # this needs to be loaded due to error on clValid
library(tictoc)

load("data/ordination_results.Rda")

# Old conventions, new names clearer
# WB dataset: pca1, mfa2
# UN dataset: pca2, mfa1

m <- "ward.D2" # minimize the total within-cluster variance

## pca1

plot(un_pca)

# using first 10 components
tic()
clust_num <- NbClust( 
    data = (un_pca$x[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all') # 3 recommended clusters
toc()
## Stability & Internal validation
stab <- clValid(
    obj = pca1$x[,1:10] ,
    nClust = 2:10,
    clMethods = c(
        "hierarchical", "kmeans",  "som", 
        "model", "diana", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan",
    method = "ward",
    verbose = FALSE
) ## Hierarchical is the optimal method

optimalScores(stab)
summary(stab)


