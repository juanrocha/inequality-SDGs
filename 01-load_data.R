library(tidyverse)
library(naniar)
library(plotly)
library(patchwork)

# time series
library(tsibble)
library(fable)
library(imputeTS)
library(slider)


## Load datasets
ecofp <- list()
fls <- list.files("data/ecofootprint/")
ecofp <- map(fls, function(x) {
    jsonlite::read_json(
        path = paste0("data/ecofootprint/", x),
        simplifyVector = TRUE)
}) %>% 
    bind_rows()

inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")

dat1 <- read_csv(
    "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") %>% 
    janitor::clean_names()

dat <- ecofp %>%
    filter(record == "EFConsPerCap") %>%
    select(countryName, isoa2, year, value) %>% 
    rename(EFconsPerCap = value, country = countryName) %>% 
    filter(year >= 1995)

dat <- dat %>% 
    left_join(
        .,
        inq %>% 
            filter(!is.na(gini_reported), year >= 1995) %>% 
            select(country, isoa2 = c2, country_code = c3, year, gini_reported) %>% 
            group_by(country, year, isoa2, country_code) %>% 
            summarize(gini_mean = mean(gini_reported, na.rm = TRUE)) 
        
    )

dat <- dat %>% 
    left_join(
        ., 
        dat1 %>% 
            select(-last_col()) %>% 
            filter(indicator_name == "GNI per capita (US$)") %>% 
            pivot_longer(cols = x1990:x2019, names_to = "year", values_to = "value") %>%
            mutate(year = str_remove(year, "x")) %>% 
            mutate(year = as.numeric(year)) %>% 
            mutate(ismissing = is.na(value)) %>% 
            filter(year >= 1995) %>% 
            rename(country = country_name, gni = value)
    )

## compute missing values
missingness <- dat %>% 
    mutate(ismissing = is.na(gini_mean)) %>% 
    group_by(country) %>% 
    summarize(missingness = sum(ismissing)/n()) %>% 
    arrange(missingness)


key_countries <- missingness %>% 
    filter(missingness <= 0.3) %>% 
    pull(country)

droped_countries <- missingness %>% 
    filter(missingness > 0.3) %>% 
    pull(country)

df_dat <- dat %>% 
    select(-starts_with("indicator")) %>% 
    filter(country %in% key_countries) %>% 
    as_tsibble(key = country, index = year) 

df_dat <- df_dat %>% 
    fill_gaps(.full = TRUE)

df_dat <- df_dat %>% 
    group_by(country) %>% 
    mutate(
        gini = na_interpolation(gini_mean),
        gni2 = na_interpolation(gni),
        EFconsPerCap = na_interpolation(EFconsPerCap)
    ) 
df_dat <- df_dat %>% 
    select(EFconsPerCap, gini, gni2) %>% 
    filter(!is.na(gni2))   # lost 2 more countries due to NAs

save(df_dat, file = "data/trilemma_cleaned_data.RData")
