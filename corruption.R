library(tidyverse)

## Corruption datasets from Gothemburg

## Standard dataset: 12.391 obs, 2008 variables
crpt <- read_csv("https://www.qogdata.pol.gu.se/data/qog_std_ts_jan25.csv")
crpt |> names()

## Basic dataset: 12.391 obs, 251 most commonly used variables
crpt <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan25.csv")
crpt

crpt |> select(year, ccodealp, ti_cpi) |> # corruption perception index
    ggplot(aes(year, ti_cpi)) +
    geom_path(aes(group = ccodealp))

crpt_dat <- crpt |> 
    select(year, ccodealp, ti_cpi) |> # corruption perception index
    group_by(ccodealp) |> 
    filter(!all(is.na(ti_cpi))) |> # remove full missing ts
    summarize(
        mean_crpt = mean(ti_cpi, na.rm = TRUE),
        trend_crpt = slope(ti_cpi)
    )

wb <- read_csv(
    "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGData.csv") %>% 
    janitor::clean_names() |> 
    filter(indicator_name == "GNI per capita (US$)") |> select(-x35) |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "gni") |> 
    mutate(year = str_remove(year, "x"), year = as.numeric(year)) |> 
    select(-indicator_code, -indicator_name) |> 
    group_by(country_code) |> 
    filter(!all(is.na(gni))) |> 
    summarize(
        mean_gni = mean(gni, na.rm = TRUE), 
        trend_gni = slope(gni)
    )
    


un_dat

slope <- function(x){
    fit <- lm(x~ seq_along(x))
    return(coefficients(fit)[2])
}

## static:
df_dat <- wb_dat |> 
    filter(!is.na(rptinc992j_p0p100), !is.na(rhweal992j_p0p100)) |> 
    #filter(country_code == "AFG") |> select(ghweal992j_p0p100, year)
    select(country_name, country_code, year,
           emissions = 'CO2 emissions (metric tons per capita)', gini_in = gptinc992j_p0p100,
           share_in = sptinc992j_p99p100, ratio_we = rhweal992j_p0p100, ratio_in = rptinc992j_p0p100,
           share_we = shweal992j_p99p100, gini_we = ghweal992j_p0p100) |> 
    group_by(country_name, country_code) |>
    summarize(
        mean_G_ineq = mean(gini_in), trend_G_ineq = slope(gini_in),
        mean_CO2 = mean(emissions), trend_CO2 = slope(emissions),
        mean_R_ineq = mean(ratio_in), trend_R_ineq = slope(ratio_in), 
        mean_S_ineq = mean(share_in), trend_S_ineq = slope(share_in),
        mean_G_we = mean(gini_we), trend_G_we = slope(gini_we),
        mean_R_we = mean(ratio_we), trend_R_we = slope(ratio_we), 
        mean_S_we = mean(share_we), trend_S_we = slope(share_we)
    ) |> 
    add_column(group = as_factor(clust_wb$Best.partition)) |> 
    left_join(crpt_dat, by = c("country_code" = "ccodealp")) |> 
    left_join(wb)

df_dat |> 
    ggplot(aes(mean_crpt, mean_G_ineq)) +
    geom_point(aes(color = trend_crpt, size = mean_CO2)) +
    geom_smooth() +
    scale_color_gradient2(mid = "grey60")

df_dat |> ggplot(aes(trend_crpt, trend_G_ineq)) +
    #geom_point(aes(color = trend_S_ineq)) +
    geom_point(aes(color = group, size = mean_CO2)) +
    #scale_color_gradient2(mid = "grey60") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_smooth(method = "lm")


df_dat |> 
    ggplot(aes(mean_crpt, mean_G_ineq)) +
    #geom_point(aes(color = trend_crpt)) +
    geom_text(aes(label = country_code, color = group), size = 1.5,
              show.legend = FALSE) +
    geom_smooth(aes(group = group), method = "lm") + #
    #scale_color_gradient2(mid = "grey80") +
    scale_color_manual(values = c("#73B3A3","#FEA621")) +
    labs(x = "Mean corruption index", y = "Mean Gini on income") +
    theme_light(base_size = 6) 


ggsave(
    plot = last_plot(),
    filename = "paper/figures/map_clusters.png", device = "png", width = 3, height = 2.5,
    bg = "white", dpi = 400)

df_dat |> ggplot(aes(mean_G_ineq, mean_G_we)) + 
    geom_point(aes(color = trend_G_ineq))+
    scale_color_gradient2(mid = "grey60") +
    geom_smooth()

df_dat |> 
    lm(formula = mean_G_ineq ~ mean_S_we + trend_S_we + mean_crpt + trend_crpt + mean_gni + trend_gni + group) |> 
    summary()
    #glm(formula = group-1 ~ mean_G_ineq + trend_G_ineq + mean_G_we + trend_G_we, family = "binomial")
    lm(formula = mean_CO2 ~ mean_G_ineq + trend_G_ineq + mean_G_we + trend_G_we + mean_crpt + trend_crpt)

fit

summary(fit)



#### maps ####
library(sf)
library(spData)
data(world)

world |> 
    left_join(
        df_dat |> 
            left_join( countries |>  select(country_code, iso_a2 = x2_alpha_code))
    ) |> 
    ggplot() +
    geom_sf(aes(fill = group), size = 0.05, color = "white", show.legend = FALSE) +
    #scale_fill_gradient2() +
    scale_fill_manual(values = c("#73B3A3","#FEA621")) +
    theme_void(base_size = 7)

wb_dat |> 
    left_join(
        crpt |> select(country_name = cname, country_code = ccodealp, year, ti_cpi )
    ) |> 
    ggplot(aes(ti_cpi, gptinc992j_p0p100)) +
    geom_path(aes(group = country))

