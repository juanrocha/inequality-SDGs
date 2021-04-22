library(tidyverse)

dat2 <- read_csv(
    "~/Documents/Projects/DATA/SDGs_UNStats/20180817102044132_juan.rocha@su.se_data.csv")

names(dat2)
#skimr::skim(dat2)


dat2 %>%
    pull(Indicator) %>%
    unique()











#### Old code from 2018 explorations ####
dat %>%
    filter(Goal == 10) %>%
    pull(SeriesDescription) %>%
    unique()


dat$GeoAreaName %>% unique()
dat$TimePeriod %>% unique()


dat %>%
    filter(Goal == 10) %>%
    group_by(GeoAreaName, SeriesDescription) %>%
    ggplot(aes(x = TimePeriod, y = Value)) +
    geom_line(aes(color = GeoAreaName), show.legend = FALSE) +
    facet_wrap(.~SeriesDescription, scales = 'free') +
    theme_light(base_size = 7)

dat %>%
    group_by(Target, GeoAreaName, TimePeriod) %>%
    summarize(na_count = sum(is.na(Value))) %>%
    arrange(desc(na_count)) %>%
    print(n = 100)
