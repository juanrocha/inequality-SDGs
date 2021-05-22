library(tidyverse)
library(naniar)


## Replicating the trilemma analysis

dat1 <- read_csv(
    "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") %>% 
    janitor::clean_names()


dat1 %>% 
    pull(indicator_name) %>%
    unique() 


dat1 %>% 
    select(-x33) %>% 
    filter(str_detect(indicator_name, "GNI")) %>% 
    pivot_longer(cols = x1990:x2017, names_to = "year", values_to = "value") %>% 
    mutate(year = str_remove(year, "x")) %>% 
    mutate(year = as.numeric(year)) %>% 
    ggplot(aes(year, value)) +
    geom_line(aes(color = country_name), show.legend = FALSE) +
    #geom_miss_point(show.legend = FALSE) +
    facet_wrap(~indicator_name, scales = "free_y")


## How many missing values? 10506, but who has more?
missingness <- dat1 %>% 
    select(-x33) %>% 
    filter(str_detect(indicator_name, "GNI")) %>% 
    pivot_longer(cols = x1990:x2017, names_to = "year", values_to = "value") %>% 
    mutate(year = str_remove(year, "x")) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(ismissing = is.na(value)) %>% 
    group_by(country_code, indicator_name, country_name) %>% 
    summarize(missingness = sum(ismissing)/n())

missingness %>% 
    ggplot(aes(country_code, indicator_name)) +
    geom_tile(aes(fill = missingness)) + 
    scale_fill_viridis_c(option = "C") +
    theme(axis.text.x = element_text(angle =90), legend.position = "top")


## Decide who to exclude and which variable to use

missingness %>% 
    ungroup() %>% 
    group_by(indicator_name) %>% 
    summarize(x = mean(missingness)) %>% 
    arrange(x)

missingness %>% 
    ungroup() %>% 
    group_by(country_name) %>% 
    summarize(x = mean(missingness)) %>% 
    arrange(desc(x)) %>% 
    print(n = 100)

## would be nice to calculate with years too.

#### Gini coefficient: source World Income Inequality database ####

inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")


inq %>% 
    #filter(resource == "Income (net/gross)") %>% 
    ggplot(aes(year, gini_reported)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    facet_grid(region_un ~ resource_detailed )

## Need to read the docs carefully, it seems many measurements per year and it's hard to tell what is the unit per time that we should use for the analysis.


#### Footprint ####

foot <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/Ecological Footprint/NFA-2021-Public-Data-Package_v1.1/NFA 2021 Public Data Package_v1.1.xlsx",
    sheet = 5, skip = 22, n_max = 207-23) %>% 
    janitor::clean_names()

## cols are grouped by production (in global ha per person), consumption (same units), and biocapacity (same units).
## Production: cols 10-15
## Consumption: cols 17-22
## biocapacity: cols 24-28
foot %>% names()

### this file is only for the last year... it does not have time series
### I've emailed the footprint network to see if we can get access to the time series.

countries <- jsonlite::read_json("data/countries.json", simplifyVector = TRUE) %>% 
    as_tibble()
