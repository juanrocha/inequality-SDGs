#devtools::install_github("DrMattG/SDGsR", dependencies = TRUE)
#library(SDGsR)

#"https://unstats.un.org/SDGAPI/swagger/../swagger/v1/swagger.json"
# Library is corrupted but one can reuse the code:


#### Download SDGs data ####
## Functions from https://github.com/DrMattG/SDGsR/tree/main/R
library(jsonlite)
library(tidyverse)

get_indicator<-function(Country, indicator){
    url<-paste0("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Indicator/Data?indicator=",
                indicator,"&areaCode=", Country)
    datcall <- jsonlite::fromJSON(url)
    page<-as.data.frame(datcall$data)
    return(page)
}

get_indicator_data<-function(indicator){
    url<-paste0("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Indicator/Data?indicator=",
                indicator,"&pageSize=50000")
    datcall <- jsonlite::fromJSON(url)
    
    indicator<-as.data.frame(datcall$data)
    
    return(indicator)
}

get_indicator_list <- function() {
    url <- c("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Indicator/List")
    datcall <- jsonlite::fromJSON(url)
    
    indicators <- data.frame(
        goal = datcall$goal,
        target = datcall$target,
        code = datcall$code,
        description = datcall$description
    )
    return(indicators)
}


## tests:
url <- "https://unstats.un.org/SDGAPI/v1/sdg/GeoArea/List"
areas <- jsonlite::fromJSON(url) |> 
    as_tibble()

indicators <- jsonlite::fromJSON("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Indicator/List") |> 
    as_tibble()

indicators |> 
    select(series) |> 
    unnest(series) |> 
    unnest(goal, target, indicator) |> 
    pull(uri) |> head()

series <- jsonlite::fromJSON("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/List") |> 
    as_tibble()

series |> 
    unnest(c(goal, target, indicator))

dat <- get_indicator_data("6.1.1") |> as_tibble()
dat
skimr::skim(dat)

## download the data safely

save_data <- function(indicator){
    d <- get_indicator_data(indicator = indicator) |> as_tibble()
    save(d, file = paste0("data/sdgs/code", str_replace_all(indicator, "\\.", "p"), ".Rda"))
}

safe_saving <- safely(save_data)

## download all indicators:
library(furrr)
plan(multisession, workers = 10)

ind <- future_map(
    indicators$code[!is_ok] ,
    .f = safe_saving,
    .progress = TRUE
)

is_ok2 <- map(transpose(ind)[[2]], is.null)
is_ok2 <- is_ok2 |> unlist()
is_ok <- is_ok |> unlist()

err <- which(!is_ok)[which(!is_ok2)]

indicators[err,"description"] 

indicators[err,"series"] |> unnest() |> 
    pull(code)


#### get series data for tables that are too big ####
get_series_data <- function(code){
    url<-paste0("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/Data?seriesCode=",
                code,"&pageSize=50000")
    datcall <- jsonlite::fromJSON(url)
    
    indicator<-as.data.frame(datcall$data)
    
    return(indicator)
}
save_data_s <- function(code){
    d <- get_series_data(code = code) |> as_tibble()
    save(d, file = paste0("data/sdgs/series-", code , ".Rda"))
}
safe_series <- safely(save_data_s)

cds <- indicators[err,"series"] |> unnest(series) |> pull(code) 

srs <- future_map(
    cds[!is_ok],
    .f = safe_series,
    .progress = TRUE
)

srs <- transpose(srs)
is_ok <- map(srs$error, is.null) |> unlist()
cds[!is_ok]

indicators[err,"series"]
plan(sequential)

## All SDGs data from the UN has been downloaded!

