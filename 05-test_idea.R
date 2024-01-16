library(tidyverse)

load("data/cleaned_trilemma-20230614.RData")

lms <- df_dat |> group_by(iso3) |> 
    summarize(model = list(lm(EFconsPerCap ~ year)))

map(.x=lms$model, ~coef(.x)[2])

lms <- lms |> 
    mutate(slope = map_dbl(model, function(x) coef(x)[2]))

df_dat |> 
    left_join(lms |> select(-model)) |> 
    ggplot(aes(gini, EFconsPerCap)) +
    geom_line(aes(group = iso3, color = slope)) +
    scale_color_viridis_c() +
    theme_light()
