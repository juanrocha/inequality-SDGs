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
    filter(goal %in% c("5", "6", "10","13","14", "15")) 

## For visualization use all the data.
tic()
dat <- dat |>
    filter(valueType != "String") |>
    mutate(value = as.numeric(value)) |> filter(!is.na(value)) |>
    filter(geoAreaName %in% df_countries$geoAreaName) #|>
# group_by(goal, target, indicator, series, geoAreaName, timePeriodStart ) |>
# summarize(value = mean(value, na.rm = TRUE))
toc() # 3s

dat |> select(-dimensions) |> skimr::skim()

zero_var <- dat |> 
    group_by(goal, target, indicator, series, geoAreaName, timePeriodStart) |> 
    summarize(variance = var(value, na.rm = TRUE)) |> 
    filter(variance == 0)


## perhaps remove zero variance here?
dat <- dat |> anti_join(zero_var)

multi_srs <- dat |> select(series, geoAreaName, timePeriodStart, value) |> 
    group_by(series, geoAreaName, timePeriodStart) |> 
    summarize(n = n()) |> 
    arrange(desc(n)) |> filter(n > 1) |> 
    pull(series) |> unique()

# There are statistics with multiple entries per year / series because they capture
# differences in gender / age / disability, etc. The main series are: multi_srs
# for example:
dat |> filter(series == "VC_VOV_GDSD", geoAreaName == "Zimbabwe", timePeriodStart == 2019) |> 
    select(seriesDescription, source, attributes, dimensions) |> 
    unnest(cols = c(attributes, dimensions)) |> pull(Location) |> unique()

un_keys |> 
    filter(series %in% multi_srs) |> 
    print(n=45)

# A lot of these time series do not make it to the final analysis due to missing values. For simplicity reduce by mean.
dat <- dat |> 
    # Because several series are used in multiple indicators, the grouping needs to start
    # at series level to avoid duplicates. If we do goal or target, there will be duplicates
    group_by(series, geoAreaName, timePeriodStart) |> 
    summarize(value = mean(value, na.rm = TRUE))


## Dont expand: this recover implicit missing values
tic()
dat <- right_join(
    # remove unimporrtant columns
    dat |>
        ungroup() |> 
        select(series, geoAreaName, timePeriodStart, value) ,
    dat |>
        ungroup() |>
        expand(series, geoAreaName, timePeriodStart)
    )
toc() # 6s

dat |> filter(is.na(value))

n_series <- dat |> pull(series) |> unique() |> length() #167 series
n_areas <- dat |> pull(geoAreaName) |> unique() |> length() #215 areas | countries | territories
n_yrs <- dat |> pull(timePeriodStart) |> unique() |> length() # 63yrs

## Test: proportion should be max 1
dat |> 
    group_by(timePeriodStart) |> 
    summarize(n= n() / (n_series * n_areas)) |> 
    ggplot(aes(timePeriodStart, n)) +
    geom_line() 

## Chose years after 2000, pretty much all the data is NAs before 2000 and after 2021
dat |> 
    group_by(timePeriodStart) |> 
    summarize(n= sum(is.na(value)) / (n_series * n_areas)) |> 
    ggplot(aes(timePeriodStart, n)) +
    geom_line() 
# try with 2005 and see if we get more than 37 countries
dat <- dat |> 
    filter(timePeriodStart >= 2005 & timePeriodStart <= 2021)

## remove time series that are all missing values
all_nas <- dat |>
    group_by(series, geoAreaName) |>
    summarize(nans = all(is.na(value))) |>
    filter(nans == TRUE)

dat <- dat |> anti_join(all_nas)

dat

l <- dat |> pull(timePeriodStart) |> unique() |> length()
df_cntrs <- dat |> 
    group_by(series, geoAreaName) |> 
    # years missing, and proportion of years missing with respect to the 22yr period
    summarize(yrs = sum(is.na(value)), missing_yrs = yrs / l) #|> 
# mutate(usable = missing_yrs < 0.2) |> 
# summarize(use = sum(usable))

#df_countries |> filter(use > 70)


df_cntrs |> 
    filter(missing_yrs < 0.3) |> 
    ggplot(aes(series, geoAreaName)) +
    geom_tile(aes(fill = missing_yrs)) +
    scale_fill_viridis_c()

df_series <- dat |> 
    group_by(series, timePeriodStart) |>
    # countries missing values, proportion of missing values wrt number of countries
    summarize(countries = sum(is.na(value)), missing_cnt = countries / n_areas) 

df_series |> 
    filter(missing_cnt < 0.3) |> 
    ggplot(aes(timePeriodStart, series)) +
    geom_tile(aes(fill = missing_cnt)) +
    scale_fill_viridis_c()

## series with 17 years or more: only allow for 
srs <- df_series |> 
    mutate(yrs_use = missing_cnt < 0.3) |> 
    summarize(yrs = sum(yrs_use)) |> #pull(yrs) |> table()
    filter(yrs > 16) |> pull(series)
# |> pull(series) %in% df_vars$series

# list of countries with complete time series in at least 25 variables
final_countries <- df_cntrs |> 
    ungroup() |> group_by(geoAreaName) |> 
    mutate(srs = missing_yrs < 0.3) |> # 30% of years is 6 yrs of the time series of 22yrs
    summarize(n = sum(srs)) |> # ggplot(aes(n)) + geom_density()
    filter(n >= 30) |> 
    pull(geoAreaName)


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
    filter(geoAreaName %in% final_countries) 

# ## combine existing values: Already done above
# dat <- dat |> 
#     group_by(series, geoAreaName, timePeriodStart) |> 
#     mutate(missing = is.na(value), obs = n())

dat |> pull(series) |> unique() |> length() # 67 series
dat |> pull(geoAreaName) |> unique() |> length() # 94 countries


## The problem persist in that there are some variables for which there are 
## missing values for all years. See plot below, and summary tables
dat |>
    group_by(series, geoAreaName) |> #filter(!series %in% prblm) |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    #filter(is.na(mean)) |> #pull(geoAreaName) |> unique()
    # ungroup() |> group_by(geoAreaName) |> 
    # summarize(n = n()) |> arrange(desc(n))
    ggplot(aes(geoAreaName, series)) +
    geom_tile(aes(fill = mean))

## an option is to remove first all series where there are multiple cases with 
## missing countries, then remove the remaining countries
prblm <- dat |> 
    group_by(series, geoAreaName) |>
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    group_by(series) |> 
    summarize(n = n()) |> # number of countries for which there is a semi-complete timeseries 
    #arrange(desc(n)) |> print(n=100)
    filter(n < 85) |> # currently 94 countries, first 18 vars gives 85 countries
    pull(series) |> unique()

prblm2 <- dat |> 
    filter(!series %in% prblm) |> 
    group_by(series, geoAreaName) |>
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    group_by(geoAreaName) |> 
    summarize(n = n()) |> #arrange(desc(n)) |> print(n=100)
    filter(n < 18) |> 
    pull(geoAreaName) |> unique()

# we lose 26 countries, with 18 time series we lose 19

srs2 <- dat |> 
    filter(!series %in% prblm)  |>
    filter(!geoAreaName %in% prblm2) |> 
    group_by(geoAreaName, series) |> 
    summarize(mean = mean(value, na.rm = TRUE)) |> 
    #ggplot(aes(geoAreaName, series)) + geom_tile(aes(fill = (mean))) # complete coverage
    pull(series) |> unique()

un_keys |> filter(series %in% srs2)

## reduce data to vars of interest: 
dat <- dat |> 
    filter(!series %in% prblm)  |> 
    filter(!geoAreaName %in% prblm2) 


## Log-transform vars with heavy tails, standardize to zero mean unit variance
dat <-  dat |> 
    mutate(value = case_when(is.nan(value) ~ NA, .default = value)) |> 
    pivot_wider(names_from = series, values_from = value) |> 
    mutate(across(.cols = all_of(srs2[c(2,3,5:10, 16,18)]), .fns = log1p )) |> 
    mutate(across(.cols = all_of(srs2), 
        .fns = function(x) { (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)})) |> 
    pivot_longer(cols = all_of(srs2), names_to = "series", values_to = "value") 

dat |> ggplot(aes(value)) + geom_density() + facet_wrap(~series, scales = 'free')

dat |> filter(is.na(value))
#### Interpolate missing values ####
## Only 38 values are NA, out of 22950
## reviewer suggest to use other method: PCA with NAs or k-means?
## I don't think k-means work because it is a timeseries, and usually the missing
## values are the same across years countries. Keeping spline given so little NAs
tic()
dat <- dat |> 
    group_by(series, geoAreaName) |> 
    mutate(value = imputeTS::na_interpolation(value, option = "spline"))
toc() # 0.5s
# test
dat |> filter(is.na(value))

un_dat <- dat

# test:
un_dat |> filter(is.na(value))


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

## reduce dataset
dat <- dat |>
    filter(indicator_name %in% vars, country_code %in% complete_countries) |> 
    select(-x35) |> 
    pivot_longer(cols = x1990:last_col(), values_to = "value", names_to = "year") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    filter(year >= 2000, year < 2019) |>  # restricting time improves
    select(-indicator_code)

dat |> ggplot(aes(value)) + geom_density() + facet_wrap(~indicator_name, scales = "free")

## log-transform and standardize
dat <- dat |> 
    pivot_wider(values_from = value, names_from = indicator_name) %>% 
    mutate(across(.cols = vars[c(1:4, 6, 7)], .fns = log1p)) |> 
    mutate(across(.cols = all_of(vars),
                  .fns = function(x) { (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}))  

dat |> pivot_longer(cols = vars, values_to = "value", names_to = "indicator_name") |>
    ggplot(aes(value)) + geom_density() + facet_wrap(~indicator_name, scales = "free")

## Interpolate missing values: using linear instead
## 633 NAs out of 27360 obs = 2.3% missing
dat |> pivot_longer(cols = vars, values_to = "value", names_to = "indicator_name") |> 
    filter(is.na(value)) 

wb_dat <- dat |> 
    pivot_longer(cols = vars, values_to = "value", names_to = "indicator_name") |> 
    group_by(country_code, year) %>% 
    mutate(value = imputeTS::na_interpolation(value, option = "spline")) 

wb_dat |> ggplot(aes(value)) + geom_density() + facet_wrap(~indicator_name, scales = "free")

wb_key <- key

wb_dat |> filter(is.na(value))

# save(un_dat, wb_dat, un_keys, wb_key, df_countries, file = "data/cleaned_SDGs_2026.Rda")
# save(wb_key, file = "data/wb_var_defs.Rda") # needed for writing
# You can combine with inequality data on the ordination step to avoid duplicating files