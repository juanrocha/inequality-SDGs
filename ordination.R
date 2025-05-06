library(tidyverse)
library(fs)
library(tictoc)
library(gganimate)
library(patchwork)
#### Import and clean data ####
## list of countries
un_countries <- read_csv2(file = "~/Documents/Projects/DATA/SDGs_UNStats/UNSD — Methodology.csv") |> 
    janitor::clean_names()
countries <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()

countries_list <- countries |> filter(!is.na(currency_unit)) |> pull(country_code)
## correct Namibia, it is not an NA in x2_alpha_code
#countries$x2_alpha_code[countries$country_code == "NAM"] <- "NA"

countries <- un_countries |># names()
    select(country_or_area:iso_alpha3_code) |> 
    filter(iso_alpha3_code %in% countries_list)


## load files
fls <- dir_ls("data/sdgs/")
# 
# sdg <- map(
#     .x = fls[1],
#     .f = function(x) load(x),
#     .progress = TRUE
# )

dat <- list()
tic()
for (i in seq_along(fls)){
    load(fls[i])
    dat[[i]] <- d
}
toc() # 21s

fls[which(map(dat, nrow) |> unlist() >= 50000)] # perhaps re-download with larger page size

tic()
dat <- dat |> bind_rows() |> 
    unnest(c(goal, target, indicator))
toc() # 12.1s

dat # 2.8M obs

keys <- dat |> 
    select(goal, target, indicator, series, seriesDescription) |> 
    unique()

df_countries <- dat |> 
    select(starts_with("geoArea")) |> 
    unique()

df_countries <- df_countries |> 
    mutate(l = str_length(geoAreaCode)) |> 
    mutate(m49_code = case_when(
        l == 1 ~ paste0("00", geoAreaCode),
        l == 2 ~ paste0("0", geoAreaCode),
        .default = geoAreaCode)
    ) |> select(-l)

un_countries <- un_countries |> 
    select(country_or_area, m49_code, contains("iso"))

### Reduce to countries only
df_countries <- un_countries |> 
    filter(iso_alpha3_code %in% countries_list) |> 
    left_join(df_countries)

## Reduce dataset to SDGs of interest. 
dat <- dat |> 
    filter(goal %in% c("5", "6", "10", "13","14", "15")) 

## For visualization use all the data.
tic()
dat <- dat |>
    filter(valueType != "String") |>
    mutate(value = as.numeric(value)) |> filter(!is.na(value)) |>
    filter(geoAreaName %in% df_countries$geoAreaName) #|>
    # group_by(goal, target, indicator, series, geoAreaName, timePeriodStart ) |>
    # summarize(value = mean(value, na.rm = TRUE))
toc() # 3s

dat |> ungroup() |> skimr::skim()

## Dont expand: this recover implicit missing values
tic()
dat <- right_join(
    dat |> select(-c(time_detail:dimensions)),
    dat |>
    ungroup() |>
    expand(series, geoAreaName, timePeriodStart))
toc() # 6s

dat |> filter(is.na(value))

dat |> pull(series) |> unique() |> length() #188 series
dat |> pull(geoAreaName) |> unique() |> length() #215 areas | countries | territories
dat |> pull(timePeriodStart) |> unique() |> length() # 63yrs


dat |> 
    group_by(timePeriodStart) |> 
    summarize(n= n()/(188*215)) |> 
    ggplot(aes(timePeriodStart, n)) +
    geom_line()

dat |> ungroup() |> 
    filter(timePeriodStart >= 2000 & timePeriodStart <= 2021) |> 
    mutate(n = 1) |> 
    group_by(geoAreaName, timePeriodStart) |> 
    summarize(obs = sum(n)) |> 
    filter(obs > 30) |> 
    ggplot(aes(timePeriodStart, geoAreaName)) +
    geom_tile(aes(fill = obs)) +
    scale_fill_viridis_c()

## Chose years >= 2000
df_cntrs<- dat |> 
    filter(timePeriodStart >= 2000 & timePeriodStart <= 2021) |> 
    group_by(series, geoAreaName) |> 
    summarize(yrs = sum(is.na(value)), missing_yrs = yrs / 22) #|> 
    # mutate(usable = missing_yrs < 0.2) |> 
    # summarize(use = sum(usable))

#df_countries |> filter(use > 70)


df_cntrs |> 
    filter(missing_yrs <0.2) |> 
    ggplot(aes(series, geoAreaName)) +
    geom_tile(aes(fill = missing_yrs)) +
    scale_fill_viridis_c()

df_series <- dat |> 
    filter(timePeriodStart >= 2000 & timePeriodStart <= 2021) |> 
    group_by(series, timePeriodStart) |> 
    summarize(countries = sum(is.na(value)), missing_cnt = countries / 215) 

df_series |> 
    filter(missing_cnt < 0.3) |> 
    ggplot(aes(timePeriodStart, series)) +
    geom_tile(aes(fill = missing_cnt)) +
    scale_fill_viridis_c()

## series with 17 years or more.
srs <- df_series |> 
    mutate(yrs_use = missing_cnt < 0.3) |> 
    summarize(yrs = sum(yrs_use)) |> 
    filter(yrs > 16) |> pull(series)
    # |> pull(series) %in% df_vars$series

# list of countries with complete time series in at least 30 variables
final_countries <- df_cntrs |> 
    ungroup() |> group_by(geoAreaName) |> 
    mutate(srs = missing_yrs < 0.3) |> # 30% of years is 6 yrs of the time series of 22yrs
    summarize(n = sum(srs)) |>#  ggplot(aes(n)) + geom_density()
    filter(n >= 30) |> pull(geoAreaName)


## compute observations, create animation with number of observations per series
# ggplot(data = dat, aes(timePeriodStart, series)) +
#     geom_tile(aes(fill = value)) +
#     scale_fill_viridis_c(na.value = "orange") +
#     labs(title = "Country: {closest_state}") +
#     transition_states(geoAreaName)
#     theme_light(base_size = 7)


## prepare data for PCA / MFA

dat <- dat |> 
    filter(series %in% srs) |> 
    filter(geoAreaName %in% final_countries) |> 
    filter(timePeriodStart >= 2000 & timePeriodStart <= 2021) |> 
    select(-goal, -target, -seriesDescription, -seriesCount, -valueType)

## combine existing values:
dat <- dat |> 
    group_by(series, geoAreaName, timePeriodStart ) |>
    summarize(value = mean(value, na.rm = TRUE),
              missing = is.na(value),
              obs = n()) 

dat |> pull(series) |> unique() |> length() # 22 series
dat |> pull(geoAreaName) |> unique() |> length() # 86 countries


## The problem persist in that there are some variables for which there are 
## missing values for all years. See plot below, and summary tables
dat |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    #filter(is.na(mean)) |> #pull(geoAreaName) |> unique()
    # ungroup() |> group_by(geoAreaName) |> 
    # summarize(n = n()) |> arrange(desc(n))
    ggplot(aes(geoAreaName, series)) +
    geom_tile(aes(fill = mean))

## an option is to remove first all series where there are multiple cases with 
## missing countries, then remove the remaining countries
prblm <- dat |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    filter(is.na(mean)) |> 
    summarize(n = n()) |> 
    filter(n >= 10) |> 
    pull(series) |> unique()

prblm2 <- dat |> 
    filter(!series %in% prblm) |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    # ggplot(aes(geoAreaName, series)) +
    # geom_tile(aes(fill = mean)) 
    filter(is.na(mean)) |> 
    pull(geoAreaName) |> unique()

# we lose 18 countries, including Sweden, Denmark, Canada, Uruguay, Bolivia

srs2 <- dat |> 
    filter(!series %in% prblm)  |>
    filter(!geoAreaName %in% prblm2) |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    #ggplot(aes(geoAreaName, series)) + geom_tile(aes(fill = (mean))) # complete coverage
    pull(series) |> unique()
    
keys |> filter(series %in% srs2)

#### Interpolate missing values ####
tic()
dat <- dat |> ungroup() |> 
    filter(!series %in% prblm)  |>
    filter(!geoAreaName %in% prblm2) |> 
    group_by(series, geoAreaName) |> 
    mutate(value = imputeTS::na_interpolation(value, option = "spline"))
toc() # 1.5s
# test
dat |> filter(is.na(value))

dat

#save(dat, keys, df_countries, file = "data/cleaned_SDGs_2025.Rda")

load("data/wii_small-data.RData")
load("data/cleaned_SDGs_2025.Rda")
ineq_dat |> pull(variable) |> unique()
## correct namibia
ineq_dat <- ineq_dat |> 
    mutate(country = case_when(is.na(country) ~ "NA", TRUE ~ country)) 

#### Ordination ####

dat <- dat |> 
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

pca1 <- dat |> 
    ungroup() |> 
    select(-geoAreaName, -country_or_area, -sptinc992j_p0p100,
           -iso_alpha2_code, -geoAreaCode, -m49_code) |> 
    filter(!is.na(rptinc992j_p0p100)) |># removes Fiji which is not on WID
    select(-shweal992j_p0p100) |> # removes zero variance 
    pivot_longer(cols = EN_LKRV_PWAC:last_col(),
                 values_to = "value", names_to = "variable") |> 
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    arrange(iso_alpha3_code) |> # 67 countries in alphabetic order - FJ
    select(-iso_alpha3_code) |> #skimr::skim() #map_dbl(.f = var)
    prcomp(center = TRUE, scale. = TRUE)

pca1

mfa1 <- dat |> 
    ungroup() |> 
    select(-geoAreaName, -country_or_area, -sptinc992j_p0p100,
           -iso_alpha2_code, -geoAreaCode, -m49_code) |> 
    filter(!is.na(rptinc992j_p0p100)) |># removes Fiji which is not on WID
    select(-shweal992j_p0p100) |># removes zero variance 
    pivot_longer(cols = EN_LKRV_PWAC:last_col(),
                 values_to = "value", names_to = "variable") |> 
    arrange(variable, year) |> #pull(year) |> unique() |> length()
    unite(col = "var", year, variable, sep = "_") |> 
    pivot_wider(names_from = var, values_from = value) |> 
    ungroup() |> 
    arrange(iso_alpha3_code) |> # 67 countries in alphabetic order - FJ
    select(-iso_alpha3_code) |> 
    as.matrix(dimnames = list(
        dat |> arrange(iso_alpha3_code) |> filter(iso_alpha3_code!="FJI") |> 
            pull(iso_alpha3_code) |> unique()
    )) |> 
    FactoMineR::MFA(group = rep(22, 25), type = rep("s", 25), ncp = 20) # 22 years times 25 variables, 

mfa1


#### Visualizations  ####
#pivot_wider(names_from = geoAreaName, values_from = value) |> 
tic()
walk(
    .x = final_countries,
    .f = function(x){
        dat_nas |> 
            filter(geoAreaName == x) |> 
            ggplot(aes(timePeriodStart, series)) +
            geom_tile(aes(fill = value)) +
            scale_fill_viridis_c(na.value = "orange") +
            labs(title = x) +
            theme_light(base_size = 7)
        
            ggsave(filename = paste0(snakecase::to_snake_case(x), ".png"),
                   device = "png", dpi = 300, plot = last_plot(), path = "figures/sdgs_country/")
    }
)
toc() # 90s



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