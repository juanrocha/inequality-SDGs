#### exploration ####
library(tidyverse)
library(skimr)
library(fs)


fls <- dir_ls("data/sdgs/")
fls

load(fls[10])

extract_summary <- function(x){
    
    load(x)
    
    x <- d |> 
        unnest(c(goal, target, indicator)) |> 
        select(goal:series, seriesDescription, source, geoAreaName:value) |> 
        #pull(source) |> head()
        mutate(timePeriodStart = as.numeric(timePeriodStart), value = as.numeric(value)) |>
        group_by(goal, target, indicator, series, geoAreaName, timePeriodStart) |>
        summarize(value = mean(value)) |> 
        ungroup()
    x |> group_by(goal, target, indicator, series) |> 
        summarize(years = max(timePeriodStart) - min(timePeriodStart),
                  countries = length(unique(geoAreaName)),
                  n = n(), 
                  completness = n/(years*countries)) |> 
        ungroup()
    
}
safe_extract <- safely(extract_summary)

out <- map(
    .x = fls,
    .f = safe_extract,
    .progress = TRUE
)

out <- transpose(out)
is_ok <- map(out$error, is.null) |> unlist()

sdgs <- out$result |> 
    bind_rows() |> 
    unique()

series <- jsonlite::fromJSON("https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/Series/List") |> 
    as_tibble()

sdgs <- sdgs |> 
    rename(seriesCode = series) |> 
    left_join(
        series |> unnest(c(goal, target, indicator)) |> 
            select(goal, target, indicator, seriesCode = code, description) |> 
            unique()
        )

write_csv(sdgs, file = "data/sdgs_completness.csv")

