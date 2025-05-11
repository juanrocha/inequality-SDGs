library(tidyverse)
library(fs)
library(tictoc)
library(gganimate)
library(patchwork)


load("data/wii_small-data.RData")
load("data/cleaned_SDGs_2025.Rda")

countries <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()


ineq_dat |> pull(variable) |> unique()
## correct namibia
ineq_dat <- ineq_dat |> 
    mutate(country = case_when(is.na(country) ~ "NA", TRUE ~ country)) 


#### UN data #####
un_dat
un_dat <- un_dat |> 
    rename(year = timePeriodStart) |> 
    select(-missing, -obs) |> 
    left_join(df_countries) |> 
    pivot_wider(names_from = series, values_from = value) |> 
    left_join(
        ineq_dat |> 
            rename(iso_alpha2_code = country, ineq = value) |> 
            filter(year >= 2000 & year <= 2021) |> 
            select(-age, -pop) |> 
            unite(col = "var", c("variable", "percentile")) |> 
            pivot_wider(names_from = var, values_from = ineq)
    ) 

# shweal has zero variance, remove

un_pca <- un_dat |> 
    ungroup() |> 
    select(-geoAreaName, -country_or_area, -iso_alpha2_code, -geoAreaCode, -m49_code) |> 
    filter(!is.na(rptinc992j_p0p100)) |># removes Fiji which is not on WID
    select(-shweal992j_p0p100, -sptinc992j_p0p100) |> # removes zero variance 
    pivot_longer(cols = EN_LKRV_PWAC:last_col(),
                 values_to = "value", names_to = "variable") |> 
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    arrange(iso_alpha3_code) |> # 67 countries in alphabetic order - FJ
    select(-iso_alpha3_code) |> #skimr::skim() #map_dbl(.f = var)
    prcomp(center = TRUE, scale. = TRUE)

un_pca

## correct dimensions here:
un_mfa <- un_dat |> 
    ungroup() |> 
    select(-geoAreaName, -country_or_area, -iso_alpha2_code, -geoAreaCode, -m49_code) |> 
    filter(!is.na(rptinc992j_p0p100)) |># removes Fiji which is not on WID
    select(-shweal992j_p0p100, -sptinc992j_p0p100) |># removes zero variance 
    pivot_longer(cols = EN_LKRV_PWAC:last_col(),
                 values_to = "value", names_to = "variable") |> 
    arrange(variable, year) |> #pull(year) |> unique() |> length()
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    arrange(iso_alpha3_code) |> # 67 countries in alphabetic order - FJ
    select(-iso_alpha3_code) |> # 550 columns = 22 years * 25 variables
    as.matrix(dimnames = list(
        un_dat |> arrange(iso_alpha3_code) |> filter(iso_alpha3_code!="FJI") |> 
            pull(iso_alpha3_code) |> unique() # 67 countries
    )) |> 
    FactoMineR::MFA(group = rep(22, 25), type = rep("s", 25), ncp = 20) # 22 years times 25 variables, 

un_mfa

#### WB data ####
wb_dat
wb_dat <- wb_dat |>
    left_join(countries |> select(country_code, country = x2_alpha_code)) |> 
    left_join(ineq_dat |> 
                  select(-age, -pop) |> 
                  unite(col = "var", c("variable", "percentile")) |> 
                  pivot_wider(names_from = var, values_from = value)
    )

# there are missing values on inequality variables, they are 99.3% complete
wb_dat |> filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> pull(country_name) |> unique()
    #summarize(allnans = all(is.na(rhweal992j_p0p100 )))

wb_pca <- wb_dat |> 
    select(-country_name, -country, -sptinc992j_p0p100, -sptinc992j_p0p100, -shweal992j_p0p100) |> 
    filter(!is.na(rptinc992j_p0p100)) |>#  pull(country_code) |> unique() |> length()
    filter(!is.na(rhweal992j_p0p100)) |> # lost 11 countries, mostly small island states lacking ineq data.
    pivot_longer(cols = `Access to electricity (% of population)`:last_col(),
                 values_to = "value", names_to = "variable") |> 
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    select(-country_code) |># skimr::skim()
    prcomp(center = TRUE, scale. = TRUE)

wb_mfa <- wb_dat |> 
    select(-country_name, -country, -sptinc992j_p0p100, -sptinc992j_p0p100, -shweal992j_p0p100) |> 
    filter(!is.na(rptinc992j_p0p100)) |>#  pull(country_code) |> unique() |> length()
    filter(!is.na(rhweal992j_p0p100)) |> # lost 11 countries, mostly small island states lacking ineq data.
    pivot_longer(cols = `Access to electricity (% of population)`:last_col(),
                 values_to = "value", names_to = "variable") |> 
    arrange(variable, year) |># pull(variable) |> unique() |> length() # 19 yrs * 15 vars
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    select(-country_code) |>  # 285 cols = 19ys*15vars
    as.matrix(dimnames = list(
        wb_dat |> filter(!is.na(rhweal992j_p0p100), !is.na(rptinc992j_p0p100)) |> 
            pull(country_name) |> unique() #149 countries
    )) |> 
    FactoMineR::MFA(group = rep(19, 15), type = rep("s", 15), ncp = 20) # 19 years times 15 variables, 

save(un_pca, un_mfa, wb_pca, wb_mfa, wb_dat, un_dat,  file = "data/ordination_results.Rda")

#### Visualizations  ####
#pivot_wider(names_from = geoAreaName, values_from = value) |> 

# tic()
# walk(
#     .x = final_countries,
#     .f = function(x){
#         dat_nas |> 
#             filter(geoAreaName == x) |> 
#             ggplot(aes(timePeriodStart, series)) +
#             geom_tile(aes(fill = value)) +
#             scale_fill_viridis_c(na.value = "orange") +
#             labs(title = x) +
#             theme_light(base_size = 7)
#         
#             ggsave(filename = paste0(snakecase::to_snake_case(x), ".png"),
#                    device = "png", dpi = 300, plot = last_plot(), path = "figures/sdgs_country/")
#     }
# )
# toc() # 90s



plot(pca1)

x <- summary(pca1, loadings = F, cutoff = 0.1)
x <- x$importance |> as_tibble(rownames = "var") |> 
    pivot_longer(cols = starts_with("PC"), names_to = "comp") |> 
    mutate(comp = str_remove(comp, "PC") |> as.numeric()) |> 
    dplyr::filter( comp <= 10 , var != "Standard deviation")

ggplot(data = x |> filter(var == "Proportion of Variance"), aes(comp, value)) +
    geom_col(color = "grey25") +
    geom_point(data = x |> filter(var == "Cumulative Proportion"), aes(comp, value)) +
    geom_line(data = x |> filter(var == "Cumulative Proportion"), aes(comp, value)) +
    labs(x = "Principal components", y = "Explained variance") +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_light(base_size = 10)

a <- pca1$x |> as_tibble() |> 
    select(PC1:PC2) |> 
    mutate(iso = dat |> arrange(iso_alpha3_code) |> filter(iso_alpha3_code!="FJI") |> 
               pull(iso_alpha3_code) |> unique() ) |> 
    ggplot(aes(PC1, PC2)) +
    geom_text(aes(label = iso), size = 3) +
    coord_equal() +
    theme_light(base_size = 12)

b <- ggplot() +
    geom_segment(
        data = pca1$rotation |>
            as_tibble(rownames = "factor") |> 
            mutate(year = str_sub(factor, start = 1L, end = 4L),
                   factor = str_remove(factor, pattern = "\\d{4}_"))  |> 
            select(year, factor, PC1:PC2),
        aes(xend = PC1, yend = PC2, color = factor, alpha = as.numeric(year)),
        x = 0, y = 0,
        arrow = arrow(length = unit(0.1,"cm")), linewidth = 0.3) +
    # scale_color_brewer("Variable",palette = "Set3",
    #                    guide = guide_legend(ncol = 4, title.position = "top")
    #                    ) +
    scale_alpha("Year", guide = guide_legend(ncol = 1, title.position = "top")) +
    coord_equal()+
    theme_light(base_size = 12)

a + b + plot_layout(guides = "collect") & 
    theme_light(base_size = 12) &
    theme(legend.position = "bottom") 


## Improve the graph, use nice names