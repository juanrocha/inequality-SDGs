## Final figures for the paper
## some chunks come from inequality-SDG note book but quality adusted for publication

library(tidyverse)
library(ggforce) # for circles
library(NbClust)
library(clValid)
library(ggnewscale)
library(gganimate)
library(patchwork)

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
m <- "ward.D2" 
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
si_wb1

## Note on interpretation (source: Multiple Factor Analysis by example using R, pg 138)
## Lg and RV are measurements that help understand relationships between variables.
## They are 0 iff each variable of one group is uncorrelated with every variable of the other.
## RV is a measurement of the relationship between two groups of multidimensional variables, in 
## cases when one group is one-dimensional, Lg is more suitable. In both measurements, we evaluate
## the importance of the structure common to two groups of variables:
## - In RV, without paying attention to the dimension of this common structure
## - In Lg, taking into account the dimension of this common structure and its inertia
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
b
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

ggsave(file = "fig_UN_ordination.png", 
       path  = "paper/figures/", device = "png",
       bg = "white", dpi = 400, width = 7, height = 4,
       plot = (aa + bb + plot_layout(guides = "collect") & 
                   theme_light(base_size = 6) &
                   theme(legend.position = "bottom") ))



#### trajectories ####
# following this: http://ggobi.github.io/ggally/articles/ggpairs.html#custom-functions
# I can create a custom function to plot trajectories and contours instead of 
# ggpairs defaults.

trajectory_plot <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
        geom_path(aes(group = country_code, color = year), alpha = 0.25) +
        geom_point(aes(color = year, group = country_code), 
                   alpha = 0.25, size = 0.5) +
        scale_colour_viridis_c()
}

contour_plot <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
        geom_density2d(...) 
}

GGally::ggpairs(
    df_dat |> 
        filter(!is.na(rptinc992j_p0p100)) |>
        select(year, starts_with("country"), everything(), -sptinc992j_p0p100),
    columns = 5:16,
    lower = list(continuous = contour_plot) ,
    upper = list(continuous = trajectory_plot)
)

# normalize all to 0-1 range to avoid complicated axis lables.
# don't plot all, select a few key vars where there is variability to explore
a <- df_dat |> #ungroup() |> skimr::skim()
    select(-sptinc992j_p0p100, -matches("Women|Individuals|Urban|Access|Forest")) |> 
    ungroup() |> #skimr::skim()
    mutate(across(matches("Cereal|CO2|Energy"), .fn = ~scales::rescale(.x, to= c(0,1))) ) |> 
    filter(!is.na(rptinc992j_p0p100)) |> 
    mutate(rptinc992j_p0p100 = rptinc992j_p0p100/100) |> #skimr::skim()
    select(year, starts_with("country"), everything()) |> 
    pivot_longer(cols = 5:10, names_to = "variable",
                 values_to = "value") |>  #skimr::skim()
    # nicer names for plotting
    mutate(variable = str_remove_all(variable, " \\(.*\\)|992j_p0p100|992j_p99p100| level of primary energy")) |> 
    pivot_wider(names_from = variable, values_from = value) |> 
    pivot_longer(cols = matches("Cereal|CO2|Energy"), names_to = "Environment", values_to = "env_values") |> 
    pivot_longer(cols = matches("tinc"), names_to = "Inequality", values_to = "inq_values") |> 
    mutate(Inequality = case_when(
        Inequality == "rptinc" ~ "Ratio 10/90",
        Inequality == "sptinc" ~ "Share top 1%",
        Inequality == "gptinc" ~ "Gini"
    )) |> 
    left_join(tibble(
        country_code = df_dat |> 
            filter(!is.na(rptinc992j_p0p100)) |> 
            pull(country_code) |> unique(),
        cluster = as.factor(clust_num$Best.partition)
    )) |> 
    ggplot(aes(inq_values, env_values)) +
    #geom_density_2d_filled() +
    geom_path(aes(group = country_code, color = cluster), linewidth = 0.15, alpha = 0.5) +
    scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red', linewidth = 0.1) +
    facet_grid(Environment ~ Inequality, scales = "free") +
    labs(x = "Inequality dimensions", y = "Enviornmental dimensions", tag = "A") +
    theme_light(base_size = 6)

# b <- df_dat |> #ungroup() |> skimr::skim()
#     select(-sptinc992j_p0p100, -matches("Women|Individuals|Urban|Access|Forest")) |> 
#     ungroup() |> #skimr::skim()
#     mutate(across(matches("Cereal|CO2|Energy"), .fn = ~scales::rescale(.x, to= c(0,1))) ) |> 
#     filter(!is.na(rptinc992j_p0p100)) |> 
#     mutate(rptinc992j_p0p100 = rptinc992j_p0p100/100) |> #skimr::skim()
#     select(year, starts_with("country"), everything()) |> 
#     pivot_longer(cols = 5:10, names_to = "variable",
#                  values_to = "value") |>  #skimr::skim()
#     # nicer names for plotting
#     mutate(variable = str_remove_all(variable, " \\(.*\\)|992j_p0p100|992j_p99p100| level of primary energy")) |> 
#     pivot_wider(names_from = variable, values_from = value) |> 
#     pivot_longer(cols = matches("Cereal|CO2|Energy"), names_to = "Environment", values_to = "env_values") |> 
#     pivot_longer(cols = matches("tinc"), names_to = "Inequality", values_to = "inq_values") |> 
#     mutate(Inequality = case_when(
#         Inequality == "rptinc" ~ "Ratio 10/90",
#         Inequality == "sptinc" ~ "Share top 1%",
#         Inequality == "gptinc" ~ "Gini"
#     )) |> 
#     left_join(tibble(
#         country_code = df_dat |> 
#             filter(!is.na(rptinc992j_p0p100)) |> 
#             pull(country_code) |> unique(),
#         cluster = as.factor(clust_num$Best.partition)
#     )) |> 
#     ggplot(aes(inq_values, env_values)) +
#     geom_density_2d_filled(show.legend = FALSE) +
#     #geom_path(aes(group = country_code, color = cluster), alpha = 0.5) +
#     #scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
#     #geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red') +
#     facet_grid(Environment ~ Inequality, scales = "free") +
#     labs(x = "Inequality dimensions", y = "Enviornmental dimensions", tag = "B") +
#     lims(y = c(0,0.3)) +
#     theme_light(base_size = 6)


ggsave(plot = a+b, filename = "bimodal.png", device = "png", path = "figures/",
       width = 7, height = 3.5, dpi = 500, bg = "white")

df_dat |> 
    filter(!is.na(rptinc992j_p0p100)) |> 
    left_join(tibble(
        country_code = df_dat |> 
            filter(!is.na(rptinc992j_p0p100)) |> 
            pull(country_code) |> unique(),
        cluster = as.factor(clust_num$Best.partition)
    )) |> 
    ggplot(aes(`CO2 emissions (metric tons per capita)`,`rptinc992j_p0p100`)) +
    geom_path(aes(group = country_code, color = cluster), alpha = 0.5) +
    geom_point(aes(color = cluster), size = 5, alpha = 0.7) +
    geom_text(aes(label = country_code), color = "white") +
    geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red') +
    scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    # new_scale_color() +
    # geom_point(aes(group = country_code, color = year), 
    #            alpha = 0.25, size = 0.5) +
    labs(title = 'Year: {closest_state}', x = "CO2 emissions", y = 'Ratio 10/90') +
    theme_light(base_size = 10) +
    transition_states(year, transition_length = 2, state_length = 1) +
    #transition_time(year) +
    ease_aes("linear")

anim_save("ineq_CO2.gif", animation = last_animation() , path = "figures/")


sm_un <- df_un_reduced |> #names()
    pivot_longer(cols = 3:173, names_to = "variable", values_to = "value") |> 
    # make nicer names:
    mutate(variable = str_remove_all(variable, "Average proportion of | Key Biodiversity Areas \\(KBAs\\) covered by protected areas \\(%\\)| under an independently verified certification scheme \\(thousands of hectares\\)|_p0p100|_p99p100|992j")) |> 
    separate(variable, into = c("year", "var"), sep = "_") |> 
    # pull(var) |> unique()
    mutate(across(matches("Forest area|Freshwater|Marine|Mountain|Terrestrial"),
                  .fn = ~scales::rescale(.x, to= c(0,1))) )  |> 
    pivot_wider(names_from = var, values_from = value) |> 
    pivot_longer(cols = matches("tinc"), names_to = "Inequality", values_to = "inq_values") |> 
    rename(`Forest area\n certified`= `Forest area certified`) |> 
    pivot_longer(cols = Freshwater:`Red List Index`, names_to = "Environment", values_to = "env_values") |> 
    mutate(Inequality = case_when(
        Inequality == "rptinc" ~ "Ratio 10/90",
        Inequality == "sptinc" ~ "Share top 1%",
        Inequality == "gptinc" ~ "Gini"
    )) |> 
    left_join(
        tibble(iso3 = df_un_reduced$iso3,
               cluster = as_factor(clust_UN$Best.partition))
    ) |> 
    ggplot(aes(inq_values, env_values)) +
    #geom_density_2d_filled() +
    geom_path(aes(group = iso3, color = cluster), linewidth = 0.15, alpha = 0.5) +
    scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red', linewidth = 0.1) +
    facet_grid(Environment ~ Inequality, scales = "free") +
    labs(x = "Inequality dimensions", y = "Enviornmental dimensions") +
    theme_light(base_size = 6)

ggsave(plot = sm_un, filename = "bimodal_UN.png", device = "png", path = "figures/",
       width = 4, height = 3.5, dpi = 500, bg = "white")



