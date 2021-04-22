library(tidyverse)

# Not working with remote access
# dat <- googlesheets4::read_sheet(
#     "https://docs.google.com/spreadsheets/d/1g2U-bMN3iAMT85x7wW1EkJ3CTvsBDq6J/edit#gid=1367510666")


dat <- readxl::read_xlsx(
    path = "data/Trilemma_StaticDynamicData.xlsx",
    sheet = "test", na = "XXXXX") #%>% janitor::clean_names()

skimr::skim(dat)
dat


GGally::ggpairs(dat, columns = 2:6)

ggplot(dat, aes(GNIpc2014)) +
    geom_density(fill = "blue", alpha = 0.5) +
    geom_vline(xintercept = c(1045, 12746), color = "red") +
    labs(title = "Gross National Income per capita", 
         caption ="Source: World Bank", x = "US dollars") +
    theme_light()

ggplot(dat, aes(`Gini2010-2015avg`)) +
    geom_density(fill = "blue", alpha = 0.5) +
    geom_vline(xintercept = c(30,40), color = "red") +
    labs(title = "Gini coefficient for income", 
         caption ="Source: World Income Inequality Database", x = "Gini") +
    theme_light()

ggplot(dat, aes(EFpc2014)) +
    geom_density(fill = "blue", alpha = 0.5) +
    #geom_vline(xintercept = c(30,40), color = "red") +
    labs(title = "Ecological footprint per capita", 
         caption ="Source: Global footprint network", x = "Ecological footprint") +
    theme_light()

ggplot(dat, aes())