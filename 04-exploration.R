library(tidyverse)

load("data/cleaned_sdg.RData")

df_dat |> 
    ggplot(aes(year, rptinc992j_p0p100)) +
    geom_line(aes(group = country_code))

df_dat |> 
    ggplot(aes(rptinc992j_p0p100, `Access to electricity (% of population)`)) +
    geom_point(aes(color = year), size = 0.5) +
    geom_line(aes(group = country), linewidth = 0.4)

df_un_reduced |>
    pivot_longer(3:last_col(), names_to = "variable", values_to = "value") |> 
    mutate(
        year = str_sub(variable, 1L, 4L),
        variable = str_sub(variable, 6L, -1L),
        year = as.numeric(year)
    ) |> 
    pivot_wider(names_from = variable, values_from = value) |> 
    ggplot(aes(`rptinc992j_p0p100`, `Average proportion of Terrestrial Key Biodiversity Areas (KBAs) covered by protected areas (%)` )) +
    geom_point(aes(color = year)) + 
    geom_line(aes(group = iso3))

ineq_sum <- df_dat |> 
    group_by(country_name) |> 
    summarize(model = lm(rptinc992j_p0p100~year))

