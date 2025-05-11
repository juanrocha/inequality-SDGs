## Final figures for the paper
## some chunks come from inequality-SDG note book but quality adusted for publication

library(tidyverse)
library(ggforce) # for circles
library(NbClust)
library(clValid)
library(mclust)
library(ggnewscale)
library(gganimate)
library(patchwork)
library(FactoMineR)
library(RColorBrewer)

load("data/cleaned_SDGs_2025.Rda") # cleaned datasets
load("data/ordination_results.Rda")
load("data/wii_small-data.RData")
countries <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()

#### multiple factor analysis ####
## World bank dataset: 
plot(wb_mfa)

wb_mfa$ind |> names()
wb_mfa$ind$contrib
# contry codes on the same order that they entered the MFA

country_codes <- wb_dat |> filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> pull(country_code) |> unique() #149 countries

wb_mfa$global.pca$eig 

# clustering:
# using first 10 components 
m <- "ward.D2" 
clust_wb <- NbClust( 
    data = (wb_mfa$ind$coord[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all') # 2 recommended clusters

## Stability & Internal validation
stab <- clValid(
    obj = wb_mfa$ind$coord[,1:10] ,
    nClust = 2,
    clMethods = c(
        "hierarchical", "kmeans",  "som", 
        "model", "diana", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan",
    method = "ward",
    verbose = FALSE
) ## Diana is the optimal method, but the Best.partition is better

optimalScores(stab)
summary(stab)

clus <- diana(
    x = wb_mfa$ind$coord[,1:10], diss = FALSE,
    stop.at.k = 2, metric = 'manhattan',
    keep.diss = FALSE, keep.data = FALSE
) |> as.hclust() |>
    cutree(k = 2)

#wb_mfa$global.pca$eig  # this shows the percentage of variance explained

a <- wb_mfa$ind$coord |> 
    as_tibble() |> 
    add_column(country_codes) |> 
    add_column(clust = as_factor(clust_wb$Best.partition)) |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_text(aes(label = country_codes, color = clust), size = 2,
              show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    labs(x = glue::glue("Dim 1 (", round(wb_mfa$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(wb_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ),
         tag = "A") +
    coord_equal() +
    theme_light(base_size = 10)
a

# recover group variable names
wb_mfa$quanti.var$coord |> rownames() |> 
    str_sub(start = 6L) |> 
    unique()

## should we use this graph on the paper or SI? possibilities: contrib, correlation
si_wb1 <- wb_mfa$group$contrib|> 
    as_tibble() |> 
    add_column(
        var_name = wb_mfa$quanti.var$coord |> rownames() |> 
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
wb_mfa$group$cos2 |> 
    as_tibble() |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_point() +
    coord_equal()

rownames(wb_mfa$quanti.var$coord)

wb_short_names <- tibble(var_name = colnames(wb_dat)[c(4:12,14:17,20,21)]) |>     
    mutate(short_name = case_when(
    var_name == "Access to electricity (% of population)" ~ "Access to electricity",
    var_name == "CO2 emissions (metric tons per capita)" ~ "CO2 emmissions",
    var_name == "Cereal yield (kg per hectare)" ~ "Cereal yield",
    var_name == "Energy intensity level of primary energy (MJ/$2011 PPP GDP)" ~ "Energy intensity",
    var_name ==  "Forest area (% of land area)" ~  "Forest area (%)",
    var_name == "Individuals using the Internet (% of population)" ~ "Internet usage",
    var_name == "Urban population (% of total population)"  ~"Urban population",
    var_name == "Women Business and the Law Index Score (scale 1-100)" ~ "Women in business",
    var_name == "ghweal992j_p0p100" ~ "Gini wealth",
    var_name == "gptinc992j_p0p100" ~ "Gini income",
    var_name == "rhweal992j_p0p100" ~ "Ratio 10/50 wealth",
    var_name == "rptinc992j_p0p100" ~ "Ratio 10/50 income",
    var_name == "shweal992j_p99p100" ~ "Share top 1% wealth",
    var_name == "sptinc992j_p99p100" ~ "Share top 1% income",
    .default = var_name
)) |> 
    mutate(goal = c(10,15,13,13,15,15,10,10,5,rep(10,6))) |> 
    mutate(short_name = fct_reorder(short_name, goal, sort)) |> 
    arrange(goal)

wb_short_names |> pull(short_name) |> levels()

# original SDG colors from: https://gist.github.com/dkdndes/7dd37498225a0461f7f52682137791c4
c("#EF402C", "#E01483", "#407F46", "#1F97D4") # blue is water but we have is land sdgs
clrs <- c("#EF402C", brewer.pal(9,"Reds"), "#407F46", "#f7fcb9" , brewer.pal(3, "Greens") )
clrs[2] <- "#E01483"
#clrs[13] <- "#1F97D4"

wb_short_names |> 
    mutate(color = clrs)

b <- wb_mfa$quanti.var$coord |> 
    as_tibble() |> 
    add_column(var_name = rownames(wb_mfa$quanti.var$coord)) |> 
    select(var_name, Dim.1, Dim.2) |># slice(172:200)
    ## avoid error on splitting:
    mutate(var_name = str_replace(var_name, "j_p", "j-p")) |> 
    separate(var_name, into = c("year", "var_name"), sep = "_" ) |> #slice(172:200) 
    mutate(var_name = str_replace(var_name, "j-p", "j_p")) |> 
    left_join(wb_short_names) |> 
    mutate(year = as.numeric(year)) |># pull(var_name) |> unique()
    ggplot() +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_segment(
        aes(xend = Dim.1, x = 0, yend = Dim.2, y = 0, color = short_name, alpha = year), 
        arrow = arrow(length = unit(0.02, "npc"))) +
    geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
    scale_color_manual("Variables from the World Bank and World Inequality databases", 
                       values = clrs,
                       guide = guide_legend(ncol = 4, title.position = "top")) +
    scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
    labs(x = glue::glue("Dim 1 (", round(wb_mfa$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(wb_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "B") +
    coord_equal() +
    theme_light(base_size = 10) +
    theme(legend.position = "bottom") 
b
# c <-  wb_mfa$ind$coord |> 
#     as_tibble() |> 
#     add_column(country_codes) |> 
#     add_column(clust = as_factor(clust_num$Best.partition)) |> 
#     ggplot(aes(Dim.3, Dim.2)) +
#     geom_text(aes(label = country_code, color = clust), size = 3,
#               show.legend = FALSE) +
#     geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
#     geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
#     scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
#     labs(x = glue::glue("Dim 3 (", round(wb_mfa$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(wb_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ),
#          tag = "C") +
#     coord_equal() +
#     theme_light(base_size = 7)

# d <- wb_mfa$quanti.var$coord |> 
#     as_tibble() |> 
#     add_column(var_name = rownames(wb_mfa$quanti.var$coord)) |> 
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
#     labs(x = glue::glue("Dim 3 (", round(wb_mfa$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(wb_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "D") +
#     coord_equal() +
#     theme_light(base_size = 7) +
#     theme(legend.position = "bottom") 


a+b + plot_layout(guides = "collect") & 
    theme_light(base_size = 10) &
    theme(legend.position = "bottom") 

ggsave(
    filename = "fig_mfa_wb.png", path = "paper/figures/", dpi = 400, bg = "white",
    device = "png", width = 7.5, height = 4,
    plot = ((a + b) + plot_layout(guides = "collect") & theme_light(base_size = 8) &
                theme(legend.position = "bottom") )
)


# testing problem with linewidth, fixed by updating ggforce with github version
#ggplot() +
#    geom_circle(aes(x0=0, y0=0, r = 1), color = "red", linewidth = 0.3 )

wb_mfa$group$coord |> 
    as_tibble()
## should we use this graph on the paper or SI? possibilities: contrib, correlation
sm_wb2 <- wb_mfa$group$coord|> 
    as_tibble() |> 
    add_column(
        var_name = wb_mfa$quanti.var$coord |> rownames() |> 
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
# using first 10 components
clust_UN <- NbClust( 
    data = (un_mfa$ind$coord[,1:10] %>% as.data.frame() %>% 
                select(where(is.numeric)) ),
    #distance = "maximum",
    min.nc = 2, max.nc = 10, 
    method = m, 
    index = 'all') # 4 recommended clusters

stab <- clValid(
    obj = un_mfa$ind$coord[,1:10] ,
    nClust = 4,
    clMethods = c(
        "hierarchical", "kmeans",  "som", 
        "model", "diana", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan",
    method = "ward",
    verbose = FALSE
) ## Diana and Agnes do equally well is the optimal method, but the Best.partition is better

optimalScores(stab)
summary(stab)

clus <- diana(
    x = un_mfa$ind$coord[,1:10], diss = FALSE,
    stop.at.k = FALSE, metric = 'euclidean',
    keep.diss = FALSE, keep.data = FALSE
) |> as.hclust() |>
    cutree(k = 4)

aa <- un_mfa$ind$coord |> 
    as_tibble() |> # remove Fiji
    add_column(country_code = un_dat |> pull(iso_alpha3_code) |> unique() %>% .[-25]) |> 
    add_column(clust = as_factor(clust_UN$Best.partition)) |> 
    ggplot(aes(Dim.1, Dim.2)) +
    geom_text(aes(label = country_code, color = clust), size = 2,
              show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    labs(x = glue::glue("Dim 1 (", round(un_mfa$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(un_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ),
         tag = "A") +
    coord_equal() +
    theme_light(base_size = 10)
aa
# recover the names and create short names
un_keys |> filter(series %in% names(un_dat)[8:34]) |> 
    arrange(series)

un_short_names <- tibble(
    var_name = names(un_dat)[c(8:30, 33:34)]
) |> arrange(var_name)
un_short_names$short_name = c(
    "Freshwater area change (%)", "Freshwater area (sq. km)", "Freshwater area (% of total land area)",
    "Seasonal water area change (%)", "Seasonal water area (sq.km)", "Seasonal water area (%)",
    "Rerservoir area (sq.km)", "Reservoir area (%)", "Level of water stress", "Water Use Efficiency",
    "Average freshwater KBA", "Average Mountain KBA", "Average Terrestrial KBA", "Red List Index",
    "Women in parliament (%)", "Woman in parliament (number)", "Pop. open defecation (%)",
    "Pop. using saniation (%)", "Refugees", "Gini wealth", "Gini income", "Ratio wealth", 
    "Ratio income", "Share 1% wealth", "Share 1% income"
)
un_short_names$goal <- c(rep(6,10), rep(15, 4), 5,5,6,6, rep(10, 7))

un_short_names <-  un_short_names |> 
    arrange(goal) |> 
    mutate(short_name = fct_reorder(short_name, goal, sort))

clrs <- c(
    brewer.pal(3, "YlOrBr")[-2] |> rev(), # SDG5
    brewer.pal(9, "Blues") |> rev(), brewer.pal(3, "PuBu"), # SDG6 water
    brewer.pal(7, 'Reds') |> rev(), # SDG10 inq
    brewer.pal(4, "Greens") |> rev() # SDG15 land
)


bb <- un_mfa$quanti.var$coord |> 
    as_tibble() |> 
    add_column(var_name = rownames(un_mfa$quanti.var$coord)) |> 
    select(var_name, Dim.1, Dim.2) |> #slice(172:200)
    mutate(var_name = str_replace(var_name, "_", "-")) |> #change the first one
    separate(var_name, into = c("year", "var_name"), sep = "-" ) |> #slice(172:200)
    mutate(year = as.numeric(year)) |> 
    left_join(un_short_names) |> 
    ggplot() +
    geom_hline(yintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.5) +
    geom_segment(
        aes(xend = Dim.1, x = 0, yend = Dim.2, y = 0, color = short_name, alpha = year), 
        arrow = arrow(length = unit(0.02, "npc"))) +
    geom_circle(aes(x0=0, y0=0, r = 1),  color = "grey24", linewidth = 0.01 ) +
    scale_color_manual("Variables from the United Nations and World Inequality databases",
                       values = clrs, #palette = "Paired",
                       guide = guide_legend(ncol = 5, title.position = "top")) +
    scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
    labs(x = glue::glue("Dim 1 (", round(un_mfa$global.pca$eig[1,2], 2), "% of variance explained)" ),
         y = glue::glue("Dim 2 (", round(un_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "B") +
    coord_equal() +
    theme_light(base_size = 10) +
    theme(legend.position = "bottom") 
bb
# cc <-  un_mfa$ind$coord |>
#     as_tibble() |>
#     add_column(country_code = df_un_reduced$iso3) |>
#     add_column(clust = as_factor(clust_UN$Best.partition)) |>
#     ggplot(aes(Dim.3, Dim.2)) +
#     geom_text(aes(label = country_code, color = clust), size = 3,
#               show.legend = FALSE) +
#     geom_vline(xintercept = 0, linetype = 2, linewidth = 0.15) +
#     geom_hline(yintercept = 0, linetype = 2, linewidth = 0.15) +
#     scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
#     labs(x = glue::glue("Dim 3 (", round(un_mfa$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(un_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ),
#          tag = "C") +
#     coord_equal() +
#     theme_light(base_size = 10)
# cc
# dd <- un_mfa$quanti.var$coord |> 
#     as_tibble() |> 
#     add_column(var_name = rownames(un_mfa$quanti.var$coord)) |> 
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
#     labs(x = glue::glue("Dim 3 (", round(un_mfa$global.pca$eig[3,2], 2), "% of variance explained)" ),
#          y = glue::glue("Dim 2 (", round(un_mfa$global.pca$eig[2,2], 2), "% of variance explained)" ), tag = "D") +
#     coord_equal() +
#     theme_light(base_size = 10) +
#     theme(legend.position = "bottom") 
# dd

ggsave(file = "fig_UN_ordination.png", 
       path  = "paper/figures/", device = "png",
       bg = "white", dpi = 400, width = 7.5, height = 5,
       plot = (aa + bb + plot_layout(guides = "collect") & 
                   theme_light(base_size = 7) &
                   theme(legend.position = "bottom") ) )

si_un1 <- un_mfa$group$contrib|> 
    as_tibble() |> 
    add_column(
        var_name = un_mfa$quanti.var$coord |> rownames() |> 
            str_sub(start = 6L) |> 
            unique()) |> 
    pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "contribution") |> 
    mutate(Dimension = str_remove(Dimension, pattern = "Dim.") |> as.numeric(),
           var_name = as_factor(var_name)) |> 
    filter(Dimension <=5) |> 
    left_join(un_short_names) |> 
    ggplot(aes(Dimension, short_name)) +
    geom_tile(aes(fill = contribution)) +
    scale_fill_viridis_c('Contribution', guide = guide_colorbar(title.position = "top", barwidth = unit(15, "mm"), barheight = unit(2, "mm"))) +
    labs(y = "Variables", x = "Dimension", tag = "A") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom") 
si_un1

sm_un2 <- un_mfa$group$coord|> 
    as_tibble() |> 
    add_column(
        var_name = un_mfa$quanti.var$coord |> rownames() |> 
            str_sub(start = 6L) |> 
            unique()) |> 
    # the commented code reproduces the default "group representation" plot
    # ggplot(aes(Dim.1, Dim.2)) +
    # geom_point(aes(color = var_name)) 
    pivot_longer(cols = starts_with("Dim"), names_to = "Dimension", values_to = "contribution") |> 
    mutate(Dimension = str_remove(Dimension, pattern = "Dim.") |> as.numeric(),
           var_name = as_factor(var_name)) |> 
    filter(Dimension <=5) |> 
    left_join(un_short_names) |> 
    ggplot(aes(Dimension, short_name)) +
    geom_tile(aes(fill = contribution)) +
    scale_fill_viridis_c('Coordinates', guide = guide_colorbar(title.position = "top", barwidth = unit(15, "mm"), barheight = unit(2, "mm"))) +
    labs(y = "", x = "Dimension", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom", axis.text.y = element_blank()) 

si_un1 + sm_un2

# ggsave(filename = "sm_un_ordinations.png", path = "paper/figures/", device = "png",
#        dpi = 400, width = 4, height = 3, bg = "white",
#        plot = si_un1 + sm_un2)
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
a <- wb_dat |> #ungroup() |> skimr::skim()
    select( -matches("Women|Individuals|Urban|Access|Forest")) |> 
    select( -sptinc992j_p0p100, -sptinc992j_p0p100, -shweal992j_p0p100) |> 
    ungroup() |> #skimr::skim()
    mutate(across(matches("Cereal|CO2|Energy"), .fn = ~scales::rescale(.x, to= c(0,1))) ) |> 
    filter(!is.na(rptinc992j_p0p100)) |> 
    mutate(rptinc992j_p0p100 = rptinc992j_p0p100/100) |> #skimr::skim()
    select(year, starts_with("country"), everything()) |> 
    pivot_longer(cols = 5:last_col(), names_to = "variable",
                 values_to = "value") |>  #skimr::skim()
    # nicer names for plotting
    mutate(variable = str_remove_all(variable, " \\(.*\\)|992j_p0p100|992j_p99p100| level of primary energy")) |> 
    pivot_wider(names_from = variable, values_from = value) |> 
    pivot_longer(cols = matches("Cereal|CO2|Energy"), names_to = "Environment", values_to = "env_values") |> 
    pivot_longer(cols = matches("tinc|hweal"), names_to = "Inequality", values_to = "inq_values") |> 
    mutate(Inequality = case_when(
        Inequality == "rptinc" ~ "Ratio 10/90\n income",
        Inequality == "sptinc" ~ "Share top 1%\n income",
        Inequality == "gptinc" ~ "Gini income",
       # Inequality == "rhweal" ~ "Ratio 10/90 wealth",
        Inequality == "shweal" ~ "Share top 1%\n wealth",
        Inequality == "ghweal" ~ "Gini wealth"
    )) |> filter(Inequality != "rhweal") |> 
    left_join(tibble(
        country_code = wb_dat |> 
            filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> 
            pull(country_code) |> unique(),
        cluster = as.factor(clust_wb$Best.partition)
    )) |> 
    ggplot(aes(inq_values, env_values)) +
    #geom_density_2d_filled() +
    geom_path(aes(group = country_code, color = cluster), show.legend = FALSE, 
              linewidth = 0.15, alpha = 0.5) +
    scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red', linewidth = 0.1) +
    facet_grid(Environment ~ Inequality, scales = "free") +
    labs(x = "Inequality dimensions", y = "Enviornmental dimensions", tag = "A") +
    theme_light(base_size = 6)
a
b <- wb_dat |> #ungroup() |> skimr::skim()
    select( -matches("Women|Individuals|Urban|Access|Forest")) |> 
    select( -sptinc992j_p0p100, -sptinc992j_p0p100, -shweal992j_p0p100) |> 
    ungroup() |> #skimr::skim()
    mutate(across(matches("Cereal|CO2|Energy"), .fn = ~scales::rescale(.x, to= c(0,1))) ) |>
    filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |>
    mutate(rptinc992j_p0p100 = rptinc992j_p0p100/100) |> #skimr::skim()
    select(year, starts_with("country"), everything()) |>
    pivot_longer(cols = 5:last_col(), names_to = "variable",
                 values_to = "value") |>  #skimr::skim()
    # nicer names for plotting
    mutate(variable = str_remove_all(variable, " \\(.*\\)|992j_p0p100|992j_p99p100| level of primary energy")) |>
    pivot_wider(names_from = variable, values_from = value) |>
    pivot_longer(cols = matches("Cereal|CO2|Energy"), names_to = "Environment", values_to = "env_values") |>
    pivot_longer(cols = matches("tinc|hweal"), names_to = "Inequality", values_to = "inq_values") |>
    mutate(Inequality = case_when(
        Inequality == "rptinc" ~ "Ratio 10/90\n income",
        Inequality == "sptinc" ~ "Share top 1%\n income",
        Inequality == "gptinc" ~ "Gini income",
        # Inequality == "rhweal" ~ "Ratio 10/90 wealth",
        Inequality == "shweal" ~ "Share top 1%\n wealth",
        Inequality == "ghweal" ~ "Gini wealth"
    )) |> filter(Inequality != "rhweal") |> 
    left_join(tibble(
        country_code = wb_dat |>
            filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |>
            pull(country_code) |> unique(),
        cluster = as.factor(clust_wb$Best.partition)
    )) |>
    ggplot(aes(inq_values, env_values)) +
    geom_density_2d_filled(show.legend = FALSE) +
    #geom_path(aes(group = country_code, color = cluster), alpha = 0.5) +
    #scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    #geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red') +
    facet_grid(Environment ~ Inequality, scales = "free") +
    labs(x = "Inequality dimensions", y = "Enviornmental dimensions", tag = "B") +
    lims(y = c(0,0.3)) +
    theme_light(base_size = 6)


ggsave(plot = a+b, filename = "bimodal.png", device = "png", path = "figures/",
       width = 7, height = 3.5, dpi = 500, bg = "white")

wb_dat |> 
    filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> 
    left_join(tibble(
        country_code = wb_dat |> 
            filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> 
            pull(country_code) |> unique(),
        cluster = as.factor(clust_wb$Best.partition)
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

## There is not enough variability in most variables to calculate contours or density plots, only for red list against some ineq indicators...
sm_un <- un_dat |> 
    select(year, matches("tinc|hwea|RSK")) |> 
    ungroup() |> rename(`Red List` = ER_RSK_LST) |> 
    pivot_longer(4:last_col(), names_to = "var_name", values_to = "values") |> 
    # make nicer names:
    left_join(un_short_names) |>
    filter(!is.na(short_name), !str_detect(short_name, "Ratio")) |> 
    # left_join(
    #     tibble(iso_alpha3_code = un_dat |>
    #                arrange(iso_alpha3_code) |> filter(iso_alpha3_code!="FJI") |> 
    #                pull(iso_alpha3_code) |> unique(),
    #            cluster = as_factor(clust_UN$Best.partition))
    # ) |> #skimr::skim()
    ggplot(aes(`Red List`, values)) +
    geom_density_2d_filled(show.legend = FALSE) +
    # geom_path(aes(group = iso_alpha3_code, color = cluster),
    #           linewidth = 0.15, alpha = 0.5) +
    # scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    # geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red', linewidth = 0.1) +
    facet_wrap(~short_name, scales = "free", nrow = 1) +
    labs(x = "Red list index", y = "Inequality dimensions", tag = "A") +
    theme_light(base_size = 7)

sm_un2 <- un_dat |> 
    select(iso_alpha3_code, year, matches("tinc|hwea|RSK")) |> 
    ungroup() |> rename(`Red List` = ER_RSK_LST) |> 
    pivot_longer(5:last_col(), names_to = "var_name", values_to = "values") |> 
    # make nicer names:
    left_join(un_short_names) |>
    filter(!is.na(short_name), !str_detect(short_name, "Ratio")) |> 
    left_join(
        tibble(iso_alpha3_code = un_dat |>
                   arrange(iso_alpha3_code) |> filter(iso_alpha3_code!="FJI") |>
                   pull(iso_alpha3_code) |> unique(),
               cluster = as_factor(clust_UN$Best.partition))
    ) |> #skimr::skim()
    ggplot(aes(`Red List`, values)) +
    #geom_density_2d_filled(show.legend = FALSE) +
    geom_path(aes(group = iso_alpha3_code, color = cluster),
              linewidth = 0.15, alpha = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = c("#73B3A3","#FEA621","#5BA4CA")) +
    geom_density2d(alpha = 0.75, show.legend = FALSE, color = 'red', linewidth = 0.1) +
    facet_wrap(~short_name, scales = "free", nrow = 1) +
    labs(x = "Red list index", y = "Inequality dimensions", tag = "B") +
    theme_light(base_size = 7)


sm_un / sm_un2
ggsave(plot = (sm_un / sm_un2), 
       filename = "bimodal_UN.png", device = "png", path = "figures/",
       width = 4, height = 3.5, dpi = 500, bg = "white")



