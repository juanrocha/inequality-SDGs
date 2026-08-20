library(tidyverse)

load("data/cleaned_SDGs_2026.Rda") # cleaned datasets
load("data/ordination_results.Rda")
load("data/wii_small-data.RData")
countries <- read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()


slope <- function(x){
    fit <- lm(x~ seq_along(x))
    return(coefficients(fit)[2])
}

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
    left_join(wb) |> 
    left_join(
        wb_mfa$ind$coord |> 
            as_tibble() |> 
            add_column(country_codes) |> 
            add_column(clust = as_factor(clust_wb$Best.partition)) |> 
            select(country_code = country_codes, clust, Dim.1:Dim.5)
    )



df_dat |> 
    ggplot(aes(mean_crpt, mean_G_ineq)) + # change for Dim.1 to use the MFA
    geom_point(aes(color = trend_crpt, size = mean_CO2)) +
    geom_smooth() +
    scale_color_gradient2(mid = "grey60")

df_dat |> ggplot(aes(trend_crpt, Dim.1)) +
    #geom_point(aes(color = trend_S_ineq)) +
    geom_point(aes(color = group, size = mean_CO2)) +
    #scale_color_gradient2(mid = "grey60") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_smooth(method = "lm")


b <- df_dat |> 
    ggplot(aes(mean_crpt, Dim.1)) +
    #geom_point(aes(color = trend_crpt)) +
    geom_text(aes(label = country_code, color = group), size = 2,
              show.legend = FALSE) +
    geom_smooth(aes(group = group), method = "lm") + #
    geom_vline(xintercept = c(35,80), color = "purple", linetype = 2, linewidth = 0.15) +
    scale_color_manual(values = c("#73B3A3","#FEA621")) +
    labs(x = "Mean corruption index", y = "MFA Dim 1", tag = "B") +
    theme_light(base_size = 10) 
b

df_dat

df_dat |> ggplot(aes(mean_G_ineq, mean_G_we)) + 
    geom_point(aes(color = trend_G_ineq))+
    scale_color_gradient2(mid = "grey60") +
    geom_smooth()

# excluding inq vars, they are colinear
fit <- df_dat |> 
    lm(formula = mean_G_ineq ~  mean_crpt + trend_crpt + mean_gni + trend_gni + group)  
    #glm(formula = group-1 ~ mean_G_ineq + trend_G_ineq + mean_G_we + trend_G_we, family = "binomial")
    #lm(formula = mean_CO2 ~ mean_G_ineq + trend_G_ineq + mean_G_we + trend_G_we + mean_crpt + trend_crpt)

c <- fit |> broom::tidy() |> 
    mutate(term = case_when(
        term == "mean_S_we" ~ "Share of 1% wealth (mean)",
        term == "trend_S_we" ~ "Share of 1% wealth (trend)",
        term == "mean_crpt" ~ "Corruption mean",
        term == "trend_crpt" ~ "Corruption trend",
        term == "mean_gni" ~ "GNI mean", 
        term == "trend_gni" ~ "GNI trend",
        term == "group2" ~ "Group 2 (yellow)", .default = term
    ), p_val = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.1 ~ "p > 0.1", .default = "p < 0.1"
    ), term = as_factor(term) |> fct_rev()) |> 
    ggplot(aes(estimate,term)) +
    geom_point(aes(fill = p_val, color = p_val), size = 1) +
    geom_errorbarh(
        aes(xmin = estimate-std.error, xmax = estimate+std.error, color = p_val),
        height = 0.3, linewidth = 0.25) +
    geom_vline(xintercept = 0, color = "grey", linetype = 2, linewidth = 0.25) +
    scale_fill_brewer(name = "P value", palette = "Set1") + 
    scale_color_brewer(name = "P value",palette = "Set1") +
    labs(y = "", tag = "C") +
    theme_light(base_size = 6) +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.8, 0.35))


summary(fit)

#save(fit, file = "data/lm_fit_Gini_corruption.Rda")

b

#### maps ####
library(sf)
library(spData)
data(world)

a <- world |> 
    left_join(
        df_dat |> 
            left_join( countries |>  select(country_code, iso_a2 = x2_alpha_code))
    ) |> 
    ggplot() +
    geom_sf(aes(fill = group), linewidth = 0.05, color = "white", show.legend = FALSE) +
    #scale_fill_gradient2() +
    scale_fill_manual(values = c("#73B3A3","#FEA621")) +
    lims(y = c(-58, NA))+ labs(tag = "A") +
    theme_void(base_size = 6)

wb_dat |> 
    left_join(
        crpt |> select(country_name = cname, country_code = ccodealp, year, ti_cpi )
    ) |> 
    left_join(df_dat) |> 
    ggplot(aes(ti_cpi, gptinc992j_p0p100)) +
    geom_path(aes(group = country_name, color = group), 
              arrow = arrow(ends = "last", length = unit(0.25, "cm"))) +
    scale_color_manual(values = c("#73B3A3","#FEA621"))



ggsave(
    plot = (a+b+c) + plot_layout(widths = c(1.5, 1,1)) ,
    filename = "paper/figures/fig_histeresis.png", device = "png", width = 7, height = 2,
    bg = "white", dpi = 400)



#### cusp experiment ####
library(cusp)
# J260707: Fiting the model to a time series is difficult because the corruption data has
# missing obs from 2005 to 2012 or so... so the two datasets are not comparable. One option is
# to reduce them both the the length for which there is corruption data, but one may lose contries.
# A more safer option is to average over time and do a static analysis instead.
# y: inequality : Dim1 in ordination OR CO2 emissions as proxy of environment and consumption
# alpha = corruption
# beta = income? OR inequality proxies
wb_dat

fit0 <- cusp(
    y ~ Dim.2, # consumption component
    alpha ~ mean_crpt, # corruption
    beta ~ Dim.1, # ineq component
    data = df_dat |> filter(!is.na(mean_crpt))
)

summary(fit0, logist = TRUE)

plot(fit0) # there is no obs on the bistability domain, but might be artifact of PCA

## check on the raw variables:

fit1 <- cusp(
    y ~ mean_CO2,
    alpha ~ mean_crpt + trend_crpt, #trend_G_we + trend_R_we + trend_S_we,
    beta ~ trend_G_ineq + trend_R_ineq + trend_S_ineq,
    data = df_dat |> filter(!is.na(mean_crpt))
)

summary(fit1)
plot(fit1)

fit2 <- cusp(
    y ~   mean_CO2,
    alpha ~  mean_S_we + trend_S_we, # mean_S_we + works
    beta ~ mean_S_ineq + trend_S_ineq, # GNI works well too
    data = df_dat |> filter(!is.na(mean_crpt))
)

summary(fit2, logist = TRUE)
plot(fit2)
cusp3d(fit2)

## encouraging: remember for interpretation logLik, higher is better; AIC | BIC: lower is better
## R^2 is misleading in when fitting cusp regressions, see JSS paper, Grasman et al 

## add the corruption time series
wb_mini <- wb_dat |> 
    left_join(
        crpt |> select(year, country_code = ccodealp, corruption = ti_cpi)
    ) |> 
    filter(!is.na(corruption), !is.na(rptinc992j_p0p100), !is.na(rhweal992j_p0p100 ))

wb_mini |> ungroup() |> skimr::skim()


tic()
fit3 <- cusp(
    y ~ `CO2 emissions (metric tons per capita)`,
    alpha ~   corruption,
    beta ~  gptinc992j_p0p100 + sptinc992j_p99p100,
    data = wb_mini
)
toc() # 626s, 10min

summary(fit3)
plot(fit3)
names(wb_mini)
op <- par()

cusp3d(fit3)

broom::tidy(fit3)
texreg::texreg(fit3)

g0 <- wb_mini |> 
    bind_cols(fit3$linear.predictors |> as_tibble()) |> 
    group_by(country) |> 
    mutate(dCO2 = c(NA, diff(`CO2 emissions (metric tons per capita)`, lag = 1))) |> 
    ggplot(aes(alpha, beta)) +
    geom_path(aes(group = country_code, color = dCO2), alpha = 1) +
    geom_point( show.legend = FALSE, alpha = 0.5) +
    scale_color_gradient2(mid = "grey")
g0

plotly::ggplotly(g0, tooltip = c("alpha", "beta", "country_code"))

# library(ggcube)
# wb_mini |> 
#     bind_cols(fit3$linear.predictors |> as_tibble()) |> 
#     ggplot(aes(alpha, beta, `CO2 emissions (metric tons per capita)`)) +
#     geom_point()


## save models for writing:
save(fit2, fit3, file = "data/cusp_fits.Rda")

## Figures for paper:
op <- par()

quartz(title = "cusp", width = 6, height =6, pointsize = 8)
par(oma = c(0,1,0,0))
plot(fit2) 
quartz.save(
    file = "paper/figures/SM_cusp_static.png", width = 6, height =6, pointsize = 8,
    type = "png", dpi = 500)
dev.off()


quartz(title = "cusp", width = 6, height =6, pointsize = 8)
par(oma = c(0,1,0,0))
plot(fit3)
quartz.save(
    file = "paper/figures/SM_cusp_dynamic.png", width = 6, height =6, pointsize = 8,
    type = "png", dpi = 500)
dev.off()

par(op)



a <- broom::tidy(fit2) |> 
    mutate(p_val = case_when(
        p.value < 0.05 ~ "P < 0.05",
        p.value > 0.1 ~ "P > 0.1", .default = "0.05 < P < 0.1"
    ) |> as_factor()) |> 
    mutate(labs = c("alpha: intercept", "alpha: mean wealth inequality","alpha: trend wealth inequality",
                    "beta: intercept","beta: mean income inequality","beta: trend income inequality",
                    "gamma: intercept","gamma: mean CO2 emissions") |> as_factor() |> fct_rev()) |> 
    ggplot(aes(estimate, labs)) +
    geom_point(aes(color = p_val), size = 2) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_val),
                   linewidth = 0.25, width = 0.2) +
    scale_color_brewer("Significance", palette = "Set1") +
    geom_vline(xintercept = 0, color = "grey", linetype = 2, linewidth = 0.5) +
    labs(y = "term", tag = "A") +
    theme_light(base_size = 7) +
    theme(legend.position.inside = c(0.2,0.67), legend.position = "inside")

b <- broom::tidy(fit3) |> 
    mutate(p_val = case_when(
        p.value < 0.05 ~ "P < 0.05",
        p.value > 0.1 ~ "P > 0.1", .default = "0.05 < P < 0.1"
    ) |> factor(levels = c("P > 0.1","0.05 < P < 0.1","P < 0.05")))  |> 
    mutate(labs = c("alpha: intercept", "alpha: corruption",
                    "beta: intercept","beta: Gini on income","alpha: Share of 1% on income",
                    "gamma: intercept","gamma: mean CO2 emissions") |> as_factor() |> fct_rev()) |> 
    ggplot(aes(estimate, labs)) +
    geom_point(aes(color = p_val), size = 2, show.legend = FALSE) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_val),
                   linewidth = 0.25, width = 0.2, show.legend = FALSE) +
    scale_color_manual("P value", values = "#4DAF4A") +
    geom_vline(xintercept = 0, color = "grey", linetype = 2, linewidth = 0.5) +
    labs(y = "term", tag = "B") +
    theme_light(base_size = 7)
b

# par(omi = c(0,0,0,0), mai = c(0,0,0,0))
# cusp3d(fit2, w = 0.02, theta = 160, phi = 35) 
# quartz.save(
#     file = "figures/cusp3d_dyn.png", type = "png", width = 2, height=2, dpi = 500
# )
# 
# cusp3d(fit3, w = 0.02, theta = 160, phi = 35)

c1 <- ggplot() +
    geom_blank() +
    theme_void(base_size = 7) +
    annotation_custom(
        grob = grid::rasterGrob(
            image = png::readPNG("figures/cusp3d_static.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

c2 <- ggplot() +
    geom_blank() +
    theme_void(base_size = 7) +
    annotation_custom(
        grob = grid::rasterGrob(
            image = png::readPNG("figures/cusp3d_dyn.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

ggsave(
    filename = "Fig_cusps.png", plot = (a + c1 )/( b + c2), device = "png",
    path = "paper/figures/", width = 6, height = 4, dpi = 500, bg = "white"
)

