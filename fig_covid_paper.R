library(tidyverse)
library(patchwork)

dat <- readxl::read_xlsx("data/international_vaccinations_0910.xlsx", sheet = 1) |> 
    janitor::clean_names()

inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")


dat <- dat |> left_join(
    inq %>% 
        filter(!is.na(gini_reported), year >= 1990) %>% 
        select(country, isoa2 = c2, country_code = c3, year, gini_reported, bottom40) %>% 
        group_by(country, isoa2, country_code) %>% 
        summarize(gini_mean = mean(gini_reported, na.rm = TRUE),
                  bottom40 = mean(bottom40, na.rm = TRUE)) 
    
)


dat |> skimr::skim()

g1 <- dat |> 
    filter(!is.na(continent)) |> 
    ggplot(aes(bottom40, vaccinated_per100)) +
    geom_point(aes(color = continent, size = gdp_per_capita_int), alpha = 0.5) +
    labs(tag = "A", y = "Fully vaccinated per 100 people", x = "Mean bottom 40% (1990-2018)") +
    scale_color_brewer("Continent", palette = "Set1") +
    scale_size("GDP per capita", range = c(0.5,3)) +
    theme_light(base_size = 6) + theme(legend.key.size = unit(0.25, "cm"))

g2 <- dat |> 
    filter(!is.na(continent)) |> 
    ggplot(aes(gini_mean, vaccinated_per100)) +
    geom_point(aes(color = continent, size = gdp_per_capita_int), alpha = 0.5)+
    labs(tag = "B", y = "Fully vaccinated per 100 people", x = "Mean GINI (1990-2018)") +
    scale_color_brewer("Continent", palette = "Set1") +
    scale_size("GDP per capita", range = c(0.5,3)) +
    theme_light(base_size = 6) + theme(legend.key.size = unit(0.25, "cm"))


g1 + g2 + plot_layout(guides = "collect") 


ggsave(
    filename = "figures/vaccination_ineq.png",
    device = "png",
    width = 6, height = 2.5,
    dpi = 300, bg = "white"
)
