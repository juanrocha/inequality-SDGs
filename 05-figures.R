## Final figures for the paper
## some chunks come from inequality-SDG note book but quality adusted for publication

library(tidyverse)
library(ggforce) # for circles
library(NbClust)
library(clValid)

load("data/cleaned_sdg.RData") # cleaned datasets
load("data/ordination_results.Rda")

# WB dataset: pca1, mfa2; data: df_dat
# UN dataset: pca2, mfa1; data: df_un_reduced

#### multiple factor analysis ####
## World bank dataset: 
plot(mfa2)

mfa2$ind |> names()
mfa2$ind$contrib
# contry codes on the same order that they entered the MFA
country_codes <- df_dat |> 
    select(-country_name, -country, -sptinc992j_p0p100) |> 
    filter(!is.na(rptinc992j_p0p100)) |> 
    pivot_longer(cols = `Access to electricity (% of population)`:last_col(),
                 values_to = "value", names_to = "variable") |> 
    arrange(variable, year) |> 
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    select(country_code)

mfa2$global.pca$eig # the first 10 components explain 94.8 of the variability

# clustering:
# using first 10 components
clust_num <- NbClust( 
    data = (mfa2$ind$coord[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all') # 3 recommended clusters

## Stability & Internal validation
stab <- clValid(
    obj = mfa2$ind$coord[,1:10] ,
    nClust = 3,
    clMethods = c(
        "hierarchical", "kmeans",  "som", 
        "model", "diana", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan",
    method = "ward",
    verbose = FALSE
) ## Diana is the optimal method, but the Best.partition is better

# clus <- diana(
#     x = mfa2$ind$coord[,1:10], diss = FALSE,
#     stop.at.k = 3, metric = 'manhattan',
#     keep.diss = FALSE, keep.data = FALSE
# ) |> as.hclust() |> 
#     cutree(k = 3)

#mfa2$global.pca$eig  # this shows the percentage of variance explained

a <- mfa2$ind$coord |> 
    as_tibble() |> 
    add_column(country_codes) |> 
    add_column(clust = as_factor(clust_num$Best.partition)) |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_text(aes(label = country_code, color = clust), size = 2,
              show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    labs(x = glue::glue("Dim 1 (", round(mfa2$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(mfa2$global.pca$eig[2,2], 2), "% of variance explained)" ),
         tag = "A") +
    coord_equal() +
    theme_light(base_size = 7)
a

# recover group variable names
mfa2$quanti.var$coord |> rownames() |> 
    str_sub(start = 6L) |> 
    unique()

## should we use this graph on the paper or SI? possibilities: contrib, correlation
si_wb1 <- mfa2$group$contrib|> 
    as_tibble() |> 
    add_column(
        var_name = mfa2$quanti.var$coord |> rownames() |> 
            str_sub(start = 6L) |> 
            unique()) |> 
    pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "contribution") |> 
    mutate(Dimension = str_remove(Dimension, pattern = "Dim.") |> as.numeric(),
           var_name = as_factor(var_name)) |> 
    filter(Dimension <=5) |> 
    ggplot(aes(Dimension, var_name)) +
    geom_tile(aes(fill = contribution)) +
    scale_fill_viridis_c('Contribution', guide = guide_colorbar(title.position = "top", barwidth = unit(15, "mm"), barheight = unit(2, "mm"))) +
    labs(y = "Variables", x = "Dimension", tag = "A") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom") 


## Note on interpretation (source: Multiple Factor Analysis by example using R, pg 138)
## Lg and RV are measurements that help understand relationships between variables.
## They are 0 iff each variable of one group is uncorrelated with every variable of the other.
## RV is a measurement of the relationship between two groups of multidimensional variables, in 
## cases when one group is one-dimensiona, Lg is more suitable. In both measurements, we evaluate
## the importance of the structure common to two groups of variables:
## - In RV, wihtout paying attention to the diminsion of this common structure
## - In Lg, taking into account the dimension of this common structure and its innertia
## relative to that of the groups. Lg is in some ways a "number of common dimensions", each
## weighted by its inertia.
mfa2$group$cos2 |> 
    as_tibble() |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_point() +
    coord_equal()


b <- mfa2$quanti.var$coord |> 
    as_tibble() |> 
    add_column(var_name = rownames(mfa2$quanti.var$coord)) |> 
    select(var_name, Dim.1, Dim.2) |> #slice(172:200)
    separate(var_name, into = c("year", "var_name"), sep = "_" ) |> #slice(172:200)
    mutate(year = as.numeric(year)) |> 
    ggplot() +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_segment(
        aes(xend = Dim.1, x = 0, yend = Dim.2, y = 0, color = var_name, alpha = year), 
        arrow = arrow(length = unit(0.02, "npc"))) +
    geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
    scale_color_brewer("Variables from the World Bank and World Inequality databases", 
                       palette = "Paired",
                       guide = guide_legend(ncol = 3, title.position = "top")) +
    scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
    labs(x = glue::glue("Dim 1 (", round(mfa2$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(mfa2$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "B") +
    coord_equal() +
    theme_light(base_size = 7) +
    theme(legend.position = "bottom") 

# c <-  mfa2$ind$coord |> 
#     as_tibble() |> 
#     add_column(country_codes) |> 
#     add_column(clust = as_factor(clust_num$Best.partition)) |> 
#     ggplot(aes(Dim.3, Dim.2)) +
#     geom_text(aes(label = country_code, color = clust), size = 3,
#               show.legend = FALSE) +
#     geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
#     geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
#     scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
#     labs(x = glue::glue("Dim 3 (", round(mfa2$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(mfa2$global.pca$eig[2,2], 2), "% of variance explained)" ),
#          tag = "C") +
#     coord_equal() +
#     theme_light(base_size = 7)

# d <- mfa2$quanti.var$coord |> 
#     as_tibble() |> 
#     add_column(var_name = rownames(mfa2$quanti.var$coord)) |> 
#     select(var_name, Dim.3, Dim.2) |> #slice(172:200)
#     separate(var_name, into = c("year", "var_name"), sep = "_" ) |> #slice(172:200)
#     mutate(year = as.numeric(year)) |> 
#     ggplot() +
#     geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
#     geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
#     geom_segment(
#         aes(xend = Dim.3, x = 0, yend = Dim.2, y = 0, color = var_name, alpha = year), 
#         arrow = arrow(length = unit(0.02, "npc"))) +
#     geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
#     scale_color_brewer("Variable",palette = "Paired",
#                        guide = guide_legend(ncol = 2, title.position = "top")) +
#     scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
#     labs(x = glue::glue("Dim 3 (", round(mfa2$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(mfa2$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "D") +
#     coord_equal() +
#     theme_light(base_size = 7) +
#     theme(legend.position = "bottom") 


a+b + plot_layout(guides = "collect") & 
    theme_light(base_size = 6) &
    theme(legend.position = "bottom") 

ggsave(
    filename = "fig_mfa_wb.png", path = "paper/figures/", dpi = 400, bg = "white",
    device = "png", width = 7, height = 4,
    plot = ((a + b) + plot_layout(guides = "collect") & theme_light(base_size = 6) &
                theme(legend.position = "bottom") )
)


# testing problem with linewidth, fixed by updating ggforce with github version
#ggplot() +
#    geom_circle(aes(x0=0, y0=0, r = 1), color = "red", linewidth = 0.3 )

mfa2$group$coord |> 
    as_tibble()
## should we use this graph on the paper or SI? possibilities: contrib, correlation
sm_wb2 <- mfa2$group$coord|> 
    as_tibble() |> 
    add_column(
        var_name = mfa2$quanti.var$coord |> rownames() |> 
            str_sub(start = 6L) |> 
            unique()) |> 
    # the commented code reproduces the default "group representation" plot
    # ggplot(aes(Dim.1, Dim.2)) +
    # geom_point(aes(color = var_name)) 
    pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "contribution") |> 
    mutate(Dimension = str_remove(Dimension, pattern = "Dim.") |> as.numeric(),
           var_name = as_factor(var_name)) |> 
    filter(Dimension <=5) |> 
    ggplot(aes(Dimension, var_name)) +
    geom_tile(aes(fill = contribution)) +
    scale_fill_viridis_c('Coordinates', guide = guide_colorbar(title.position = "top", barwidth = unit(15, "mm"), barheight = unit(2, "mm"))) +
    labs(y = "", x = "Dimension", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom", axis.text.y = element_blank()) 

si_wb1 + sm_wb2

# ggsave(filename = "sm_wb_ordinations.png", path = "paper/figures/", device = "png",
#        dpi = 400, width = 4, height = 3, bg = "white",
#        plot = si_wb1 + sm_wb2)

## UN dataset
# country codes is df_un_reduced$iso3
# using first 10 components
clust_UN <- NbClust( 
    data = (mfa1$ind$coord[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all') # 3 recommended clusters


aa <- mfa1$ind$coord |> 
    as_tibble() |> 
    add_column(country_code = df_un_reduced$iso3) |> 
    add_column(clust = as_factor(clust_UN$Best.partition)) |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_text(aes(label = country_code, color = clust), size = 2,
              show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    labs(x = glue::glue("Dim 1 (", round(mfa1$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(mfa1$global.pca$eig[2,2], 2), "% of variance explained)" ),
         tag = "A") +
    coord_equal() +
    theme_light(base_size = 7)
aa


bb <- mfa1$quanti.var$coord |> 
    as_tibble() |> 
    add_column(var_name = rownames(mfa1$quanti.var$coord)) |> 
    select(var_name, Dim.1, Dim.2) |> #slice(172:200)
    separate(var_name, into = c("year", "var_name"), sep = "_" ) |> #slice(172:200)
    mutate(year = as.numeric(year)) |> 
    ggplot() +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_segment(
        aes(xend = Dim.1, x = 0, yend = Dim.2, y = 0, color = var_name, alpha = year), 
        arrow = arrow(length = unit(0.02, "npc"))) +
    geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
    scale_color_brewer("Variables from the United Nations and World Inequality databases",palette = "Paired",
                       guide = guide_legend(ncol = 2, title.position = "top")) +
    scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
    labs(x = glue::glue("Dim 1 (", round(mfa1$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(mfa1$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "B") +
    coord_equal() +
    theme_light(base_size = 7) +
    theme(legend.position = "bottom") 
bb
# cc <-  mfa1$ind$coord |> 
#     as_tibble() |> 
#     add_column(country_code = df_un_reduced$iso3) |> 
#     add_column(clust = as_factor(clust_UN$Best.partition)) |> 
#     ggplot(aes(Dim.3, Dim.2)) +
#     geom_text(aes(label = country_code, color = clust), size = 3,
#               show.legend = FALSE) +
#     geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
#     geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
#     scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
#     labs(x = glue::glue("Dim 3 (", round(mfa1$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(mfa1$global.pca$eig[2,2], 2), "% of variance explained)" ),
#          tag = "C") +
#     coord_equal() +
#     theme_light(base_size = 10)
# cc
# dd <- mfa1$quanti.var$coord |> 
#     as_tibble() |> 
#     add_column(var_name = rownames(mfa1$quanti.var$coord)) |> 
#     select(var_name, Dim.3, Dim.2) |> #slice(172:200)
#     separate(var_name, into = c("year", "var_name"), sep = "_" ) |> #slice(172:200)
#     mutate(year = as.numeric(year)) |> 
#     ggplot() +
#     geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
#     geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
#     geom_segment(
#         aes(xend = Dim.3, x = 0, yend = Dim.2, y = 0, color = var_name, alpha = year), 
#         arrow = arrow(length = unit(0.02, "npc"))) +
#     geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
#     scale_color_brewer("Variable",palette = "Paired",
#                        guide = guide_legend(ncol = 2, title.position = "top")) +
#     scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
#     labs(x = glue::glue("Dim 3 (", round(mfa1$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(mfa1$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "D") +
#     coord_equal() +
#     theme_light(base_size = 10) +
#     theme(legend.position = "bottom") 
# dd

ggsave(file = "fig_UN_ordination.png", path  = "paper/figures/", device = "png",
       bg = "white", dpi = 400, width = 7, height = 4,
       plot = (aa + bb + plot_layout(guides = "collect") & 
                   theme_light(base_size = 6) &
                   theme(legend.position = "bottom") ))


#### PCA ####
pca1
