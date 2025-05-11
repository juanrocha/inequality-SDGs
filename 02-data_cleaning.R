library(tidyverse)
library(fs)
library(tictoc)
library(gganimate)
library(patchwork)

#### UN SDGs ####
##list of countries
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

un_keys <- dat |> 
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

un_keys |> filter(series %in% srs2)

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

un_dat <- dat

#### World Bank ####
## copied from notebook, here for faster manipulation

dat <- read_csv(file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") |> 
    janitor::clean_names()

dat

key <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1T6rZ5T1qL4BPDL5oatMf3y8lLzhnIDvxIvkbJeBkdUc/edit?usp=sharing", sheet = 1
) |> janitor::clean_names()

key |> pull(topic) |> unique()

countries <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()

countries_list <- countries |> filter(!is.na(currency_unit)) |> pull(country_code)
## correct Namibia, it is not an NA in x2_alpha_code

countries$x2_alpha_code[countries$country_code == "NAM"] <- "NA"

### visualizations are kept on notebook, most of the where for SM
maike_vars <- key |> 
    filter(!is.na(maike_selection)) |> 
    pull(indicator_name) |> 
    unique()

n_countries <- dat |> filter(country_code %in% countries_list) |> 
    pull(country_code) |> unique() |> length()

other_vars <- key |> 
    filter(!is.na(include) | !is.na(maike_selection)) |> 
    pull(indicator_name) |> 
    unique()

p <- dat |>
    filter(indicator_name %in% other_vars, country_code %in% countries_list) |> 
    select(-x35) |> 
    pivot_longer(cols = x1990:last_col(), values_to = "value", names_to = "year") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    group_by(indicator_name, year) |> 
    mutate(nas = is.na(value)) |> 
    summarise(missing = sum(nas) / n_countries) |> 
    ungroup() |> group_by(indicator_name) |> 
    summarize(mean_missing = mean(missing)) |> 
    filter(mean_missing < 0.3) |> 
    ggplot(aes(mean_missing, indicator_name)) +
    geom_point() +
    #scale_fill_viridis_c(
    #    guide = guide_colorbar(barwidth = unit(2,"mm"), barheight = unit(20, "mm"))) + 
    labs(x = "Mean proportion of missing values across countries", y = "Indicator name", tag = "A")+
    theme_light(base_size = 8)

vars <- p$data$indicator_name

complete_countries <-  dat |>
    filter(indicator_name %in%  vars) |>
    select(-x35) |>
    pivot_longer(cols = x1990:last_col(), values_to = "value", names_to = "year") |>
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |>
    mutate(nas = is.na(value)) |>
    filter(year >= 2000, year < 2019) |>  # restricting time improves
    group_by(country_code, indicator_name) |>
    summarise(missing = sum(nas) / diff(range(year))) |> 
    # filter out any country with missing values higher than 70%
    filter(!any(missing > 0.3)) |> 
    pull(country_code) |> unique()

## Interpolate missing values
## 
wb_dat <- dat |>
    filter(indicator_name %in% vars, country_code %in% complete_countries) |> 
    select(-x35) |> 
    pivot_longer(cols = x1990:last_col(), values_to = "value", names_to = "year") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    filter(year >= 2000, year < 2019) |>  # restricting time improves
    select(-indicator_code) |> 
    pivot_wider(values_from = value, names_from = indicator_name) %>% 
    group_by(country_code) %>% 
    mutate(across(.cols = vars, .fns = imputeTS::na_interpolation, option = "spline")) 

wb_key <- key

# save(un_dat, wb_dat, un_keys, wb_key, df_countries, file = "data/cleaned_SDGs_2025.Rda")
# save(wb_key, file = "data/wb_var_defs.Rda") # needed for writing
# You can combine with inequality data on the ordination step to avoid duplicating files