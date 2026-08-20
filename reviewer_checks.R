library(tidyverse)


wb_inq <- readxl::read_xlsx("~/Downloads/inequality GMD World Bank.xlsx")

wb_inq |> skimr::skim()

wb_inq |> 
    group_by(countryname) |> 
    summarize(min_yr = min(year), max_yr = max(year)) |> 
    mutate(n_yrs = max_yr-min_yr) |> 
    arrange(desc(n_yrs)) |> 
    print(n = 108)
