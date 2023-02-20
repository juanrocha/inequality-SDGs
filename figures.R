library(ggplot2)
library(patchwork)
library(plot3D)
library(magrittr)


load("data/cleaned_trilemma.RData")

#### Conceptual figure ####
old_par <- par()

quartz(width = 2.5, height = 2.5, pointsize = 7)
par(mar = c(0,0,0,0))
box3D(
    x0 = -0.8, y0 = -0.8, z0 = -0.8, 
    x1 = 0.8, y1 = 0.8, z1 = 0.8, 
    border = "black", lwd = 1, 
    col = gg.col(1, alpha = 0.25), 
    xlab = "Inequality", ylab = "Environment",
    zlab = "Prosperity", cex.lab = 1.2, mar = c(0,0,0,0))

quartz.save(
    file="figures/cube_concept.png", type = "png", dpi = 300,
    width = 2.5, height = 2.5, pointsize = 7, bg = "white"
)
dev.off()
par(old_par)


p1 <- ggplot() +
    geom_blank() + 
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5), 
              fill = "skyblue", alpha = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1), 
              fill = "orange", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5), 
              fill = "orange", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1), 
              fill = "red", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    geom_abline(intercept = 1, slope = -1, color = "red") +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    geom_vline(xintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    annotate("text", x = 0.15, y = 0.2, label = "Synergies", size = 2,
             color = "blue", angle = 45) +
    annotate("text", x = 0.15, y = 0.9, label = "Trade-off", size = 2,
             color = "red", angle = 360-45) +
    lims(x = c(0,1), y = c(0,1)) +
    labs(x = "Inequality", y = "Ecological footprint", tag = "D") +
    coord_equal() +
    theme_light(base_size = 7) +
    theme(axis.text = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank())


p2 <- ggplot() +
    geom_blank() + 
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5), 
              fill = "orange", alpha = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1), 
              fill = "skyblue", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5), 
              fill = "red", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1), 
              fill = "orange", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_abline(intercept = 1, slope = -1, color = "blue") +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    geom_vline(xintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    annotate("text", x = 0.15, y = 0.2, label = "Trade-off", 
             size = 2, color = "red", angle = 45) +
    annotate("text", x = 0.15, y = 0.9, label = "Synergies", 
             size = 2, color = "blue", angle = 360-45) +
    lims(x = c(0,1), y = c(0,1)) +
    labs(x = "Inequality", y = "Prosperity", tag = "C") +
    coord_equal() +
    theme_light(base_size = 7) +
    theme(axis.text = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank())

p3 <- ggplot() +
    geom_blank() + 
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5), 
              fill = "orange", alpha = 0.5) +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1), 
              fill = "skyblue", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5), 
              fill = "red", alpha = 0.5) +
    geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1), 
              fill = "orange", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_abline(intercept = 1, slope = -1, color = "blue") +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    geom_vline(xintercept = 0.5, linetype = 2, color = "grey50", size = 0.5) +
    annotate("text", x = 0.15, y = 0.2, size = 2,
             label = "Trade-off", color = "red", angle = 45) +
    annotate("text", x = 0.15, y = 0.9, label = "Synergies", size = 2,
             color = "blue", angle = 360-45) +
    lims(x = c(0,1), y = c(0,1)) +
    labs(x = "Ecological footprint", y = "Prosperity", tag = "B") +
    coord_equal() +
    theme_light(base_size = 7) +
    theme(axis.text = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank())

p0 <- ggplot() +
    geom_blank() +
    theme_void(base_size = 7) +
    annotation_custom(
        grob = grid::rasterGrob(
            image = png::readPNG("figures/cube_concept.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    labs(tag = "A")
  
p6 <- df_dat %>% 
    ggplot(aes( gini,EFconsPerCap , group = country)) +
    geom_point(size = 0.15,aes(color = year), show.legend = FALSE) +
    geom_path(size = 0.1, aes(color = year)) +
    annotate(geom = "rect", ymin = 0, ymax = 1.68, xmin = 20, xmax = 30,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom ="rect",ymin = 0, ymax = 1.68, xmin = 30, xmax = 62,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect", ymin = 1.68, ymax = 18, xmin = 20, xmax = 30,
             fill = "orange", alpha = 0.2) +
    annotate(geom="rect", ymin = 1.68, ymax = 18, xmin = 30, xmax = 62,
             fill = "red", alpha = 0.2) +
    geom_hline(yintercept = 1.68, linetype = 2, color = "gray50", size = 0.1) +
    geom_vline(xintercept = 30, linetype = 2, color = "gray50", size = 0.1) +
    #scico::scale_color_scico("Year", palette = "bilbao") +
    scale_color_viridis_c("Year",direction = -1) +
    labs(x = "Inequality (Gini)", y = "Ecological footprint", tag = "G") + theme_light() +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm"))

p5 <- df_dat %>% 
    ggplot(aes( gini, gni2, group = country)) +
    geom_point(size = 0.15, show.legend = FALSE, aes(color = year)) +
    geom_path(size = 0.1, aes(color = year)) +
    annotate(geom = "rect", ymin = 10, ymax = 12746, xmin = 20, xmax = 30,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect",ymin = 10, ymax = 12746, xmin = 30, xmax = 62,
             fill = "red", alpha = 0.2) +
    annotate(geom ="rect", ymin = 12746, ymax = 105000, xmin = 20, xmax = 30,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom="rect", ymin = 12746, ymax = 105000, xmin = 30, xmax = 62,
             fill = "orange", alpha = 0.2) +
    geom_hline(yintercept = 12746, linetype = 2, color = "gray50", size = 0.1) +
    geom_vline(xintercept = 30, linetype = 2, color = "gray50", size = 0.1) +
    # scico::scale_color_scico("Year", palette = "bilbao") +
    scale_color_viridis_c("Year",direction = -1) +
    scale_y_log10() +
    labs(x = "Inequality (Gini)", y = "Prosperity (GNI [log])", tag = "F") + theme_light() +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm"))


p4 <- df_dat %>% 
    ggplot(aes( EFconsPerCap, gni2, group = country)) +
    geom_point(size = 0.15, show.legend = FALSE, aes(color = year)) +
    geom_path(size = 0.1, aes(color = year))+
    annotate(geom = "rect", ymin = 10, ymax = 12746, xmin = 0, xmax = 1.68,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect",ymin = 10, ymax = 12746, xmin = 1.68, xmax = 18,
             fill = "red", alpha = 0.2) +
    annotate(geom ="rect", ymin = 12746, ymax = 105000, xmin = 0, xmax = 1.68,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom="rect", ymin = 12746, ymax = 105000, xmin = 1.68, xmax = 18,
             fill = "orange", alpha = 0.2) +
    geom_hline(yintercept = 12746, linetype = 2, color = "gray50", size = 0.1) +
    geom_vline(xintercept = 1.68, linetype = 2, color = "gray50", size = 0.1) +
    # scico::scale_color_scico("Year", palette = "bilbao") +
    scale_color_viridis_c("Year",direction = -1, option = "D") +
    scale_y_log10() +
    labs(x = "Ecological footprint", y = "Prosperity (GNI [log])", tag = "E") + theme_light() +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm"))

# ggsave(
#     plot = p1 + p2 + p3 + plot_layout(guides = "collect") & 
#         theme_light(base_size = 7) & 
#         theme(legend.position = "bottom", panel.grid = element_blank(),
#           legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm")),
#     path = "figures/", file = "cube_time.png", device = "png", dpi = 300,
#     width = 7, height = 3, bg = "white"
# )


(p0 + p3 + p2 + p1 + plot_layout(ncol = 4)) /
(p4 + p5 + p6 + plot_layout(guides = "collect") & theme_light(base_size = 7) & 
    theme(legend.position = "bottom", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(2,"mm")))
`

ggsave(
    plot = ((p0 + p3 + p2 + p1 + plot_layout(ncol = 4)) /
                (p4 + p5 + p6 + plot_layout(guides = "collect") & theme_light(base_size = 7) & 
                     theme(legend.position = "bottom", panel.grid = element_blank(),
                           legend.key.width = unit(10, "mm"), legend.key.height = unit(2,"mm")))),
    filename = "fig1_trilemma_space.png",
    path = "figures/", device = "png", 
    width = 6, height = 4, bg = "white", dpi = 400
)

#### Ensamble ####

c3 <- df_dat |> 
    ggplot(aes( gini, EFconsPerCap)) +
    # stat_density_2d(
    #     geom = "raster", aes(fill = after_stat(density)), alpha = 0.85, contour = FALSE) +
    # scale_fill_viridis_c() + 
    #geom_density_2d(color = "white", size = 0.2) +
    geom_density_2d_filled(alpha = 0.95, show.legend = FALSE) +
    geom_point(aes(color = gni_log), size = 0.01, alpha = 0.25) +
    geom_path(aes(group = country, color = gni_log), size = 0.05, alpha = 0.75) +
    scico::scale_color_scico(
        "Prosperity (GNI [log])", palette = "grayC", direction = -1,
        guide = guide_colorbar(title.position = "top")) +
    geom_hline(yintercept = 1.68, linetype = 2, color = "gray90", size = 0.2) +
    geom_vline(xintercept = 30, linetype = 2, color = "gray90", size = 0.2) +
    labs(x = "Inequality (Gini)", y = "Ecological footprint") +
    theme_light(base_size = 6) +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(7, "mm"), legend.key.height = unit(2,"mm"))

c2 <- df_dat |> 
    ggplot(aes(gini,gni2)) +
    geom_density_2d_filled(alpha = 0.95, show.legend = FALSE) +
    geom_point(aes(color = EFconsPerCap), size = 0.01, alpha = 0.25) +
    geom_path(aes(group = country, color = EFconsPerCap), size = 0.05, alpha = 0.75) +
    geom_hline(yintercept = (12546), linetype = 2, color = "gray90", size = 0.2) +
    geom_vline(xintercept = 30, linetype = 2, color = "gray90", size = 0.2) +
    scico::scale_color_scico(
        "Ecological footprint",palette = "grayC", direction = 1,
        guide = guide_colorbar(title.position = "top")) +
    labs(y= "Prosperity (GNI [log])", x = "Inequality (Gini)") +
    scale_y_log10() +
    theme_light(base_size = 6) +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(7, "mm"), legend.key.height = unit(2,"mm"))

c1 <- df_dat |> 
    ggplot(aes(y = gni2, x = EFconsPerCap)) +
    geom_density_2d_filled(alpha = 0.95, show.legend = FALSE) +
    geom_point(aes(color = gini), size = 0.01, alpha = 0.25) +
    geom_path(aes(group = country, color = gini), size = 0.05, alpha = 0.75) +
    scico::scale_color_scico(
        "Inequality (Gini)",palette = "grayC", direction = 1,
        guide = guide_colorbar(title.position = "top")) +
    labs(y = "Prosperity (GNI)", x = "Ecological footprint") +
    #scale_y_log10()+
    geom_hline(yintercept = (12546), linetype = 2, color = "gray90", size = 0.2) +
    geom_vline(xintercept = 1.68, linetype = 2, color = "gray90", size = 0.2) +
    theme_light(base_size = 6) +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(7, "mm"), legend.key.height = unit(2,"mm"))

## After running the clustering routine in the notebook, you can recreate the insets
## Clustering
## 
## 
a1 <- df_dat |> 
    as_tibble() |> 
    group_by(country) |> 
    dplyr::summarize(
        EF = sum(d_EF, na.rm = TRUE),
        Gini = sum(d_gini, na.rm = TRUE),
        GNI = sum(d_gni, na.rm = TRUE)
    ) |> 
    mutate(Group = as.character(stab@clusterObjs$kmeans$`3`$cluster)) |> 
    left_join(
        dat |> select(country, country_code) |> 
            filter(!is.na(country_code)) |> unique()) |> 
    ggplot(aes(y = EF,x= Gini)) +
    geom_segment(
        aes(x = 0, y = 0, yend = EF, xend = Gini, color = as.factor(Group)),
        arrow = arrow(length = unit(0.1, "cm")), show.legend = FALSE, size = 0.15) +
    geom_text(
        aes(y = EF *1.1, x = Gini * 1.1, label = country_code, colour = Group), 
        size = 1, show.legend = FALSE) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    labs(y = "Ecological Footprint", x = "Inequality (Gini)") +
    theme_light(base_size = 5) +
    theme(panel.grid = element_blank(), legend.position = c(0.8,0.2), 
          legend.text = element_text(size = 4), legend.title = element_text(size =4),
          legend.key.width = unit(2,"mm"), legend.key.height = unit(2, "mm"))    


a1

f3 <- c3 + annotation_custom(
    grob = ggplotGrob(a1),
    ymin = 9, ymax = 17, xmin = 40, xmax = 60
)  

# ggsave(
#     filename = "Gini_EF.png", path = "figures/", device = "png",
#     width = 4, height = 4, dpi = 300, bg = "white", 
#     plot = f3
# )


a2 <- df_dat |> 
    as_tibble() |> 
    group_by(country) |> 
    dplyr::summarize(
        EF = sum(d_EF, na.rm = TRUE),
        Gini = sum(d_gini, na.rm = TRUE),
        GNI = sum(d_gni, na.rm = TRUE)
    ) |> 
    mutate(Group = as.character(stab@clusterObjs$kmeans$`3`$cluster)) |> 
    left_join(
        dat |> select(country, country_code) |> 
            filter(!is.na(country_code)) |> unique()) |> 
    ggplot(aes(y = GNI, x = Gini)) +
    geom_segment(
        aes(x = 0, y = 0, yend = GNI, xend = Gini, 
            color = as.factor(Group)), show.legend = FALSE,
        arrow = arrow(length = unit(0.1, "cm")), size = 0.15) +
    geom_text(aes(y = GNI * 1.1, x = Gini *1.1, 
                  label = country_code, colour = Group), 
              size = 1, show.legend = FALSE) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    theme_light(base_size = 5) + labs(y = "Prosperity (GNI [log])", x = "Inequality (Gini)") +
    theme(panel.grid = element_blank(), legend.position = c(0.8,0.2),
          legend.text = element_text(size = 4), legend.title = element_text(size = 4),
          legend.key.width = unit(2,"mm"), legend.key.height = unit(2, "mm")) 

f2 <- c2 + annotation_custom(
    grob = ggplotGrob(a2),
    ymin = 0.85, ymax = 2.6, xmin = 40, xmax = 60
) 
# ggsave(
#     filename = "Gini_GNI.png", path = "figures/", device = "png",
#     width = 4, height = 4, dpi = 300, bg = "white", 
#     plot = f2
# )


a3 <- df_dat |> 
    as_tibble() |> 
    group_by(country) |> 
    dplyr::summarize(
        EF = sum(d_EF, na.rm = TRUE),
        Gini = sum(d_gini, na.rm = TRUE),
        GNI = sum(d_gni, na.rm = TRUE)
    ) |> 
    mutate(Group = as.character(stab@clusterObjs$kmeans$`3`$cluster)) |> 
    left_join(
        dat |> select(country, country_code) |> 
            filter(!is.na(country_code)) |> unique()) |> 
    ggplot(aes(y = GNI,x = EF)) +
    geom_segment(
        aes(x = 0, y = 0, yend = GNI, xend = EF, 
            color = as.factor(Group)), show.legend = FALSE, 
        arrow = arrow(length = unit(0.1, "cm")), size = 0.15) +
    geom_text(aes(y = GNI * 1.1, x = EF *1.1, 
                  label = country_code, colour = Group), 
              size = 1, show.legend = FALSE) +
    scale_color_manual("Groups", values = c("#73B3A3","#FEA621","#5BA4CA")) +
    theme_light(base_size = 5) + labs(x = "Ecological footprint", y = "Prosperity (GNI)") +
    theme(panel.grid = element_blank(), legend.position = c(0.8,0.8),
          legend.text = element_text(size = 4), legend.title = element_text(size = 4),
          legend.key.width = unit(2,"mm"), legend.key.height = unit(2, "mm")) 


f1 <- c1 + annotation_custom(
    grob = ggplotGrob(a3),
    ymin = 1, ymax = 2.85, xmin = 9, xmax = 17.8
) 

# ggsave(
#     filename = "EF_GNI.png", path = "figures/", device = "png",
#     width = 4, height = 4, dpi = 300, bg = "white", 
#     plot = f1
# )


### ensamble

ggsave(
    filename = "trilema_ensamble.png", device = "png", path = "figures/",
    width = 7.5, height = 3, bg = "white", dpi = 300,
    plot = f1 + labs(tag = "A") + f2 + labs(tag = "B") +  f3 + labs(tag = "C")
)

#### surface ####


with(df_dat |> filter(country != "Luxembourg"), {
    
    #linear regression:
    fit <- glm(EFconsPerCap  ~ gni2 + gini + I(gni2^2) + I(gini^2), 
               family = "gaussian", data = df_dat)
    
    ## predict values
    gni_pred <- seq(10,104010, length.out = 30) # in log units 2.3,11.5
    gini_pred <- seq(21,61, length.out = 30)
    xy <- expand.grid(gni2 = gni_pred, gini = gini_pred)
    EF_pred <- matrix(
        nrow = 30, ncol = 30, data = predict(
            fit, newdata = data.frame(xy), interval = "prediction"))
    
    ## plot
    scatter3D(
        z = EFconsPerCap, y = gini, x = gni2, pch = 1, theta = 0, phi = 20, 
        ticktype = "detailed", zlab = "Ecological footprint", ylab = "Inequality (Gini)",
        xlab = "Prosperity (GNI[log])",
        surf = list(z = EF_pred, y = gini_pred, x = gni_pred, alpha = 0.5, col = "grey40")
    )
})

with(df_dat |> filter(country != "Luxembourg"), {
    scatter3D(
        z = EFconsPerCap, y = gini, x = gni2, pch = 1, theta = -50, phi = 20, 
        ticktype = "detailed", zlab = "Ecological footprint", ylab = "Inequality (Gini)",
        xlab = "Prosperity (GNI[log])")
})


fit <- lm(EFconsPerCap  ~ gni2 + gini + I(gni2^4) + I(gini^4) + gni2*gini, 
            data = df_dat)
summary(fit)
