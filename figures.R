library(ggplot2)
library(patchwork)

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
    labs(x = "Inequality", y = "Environmental footprint", tag = "D") +
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
    labs(x = "Environmental footprint", y = "Prosperity", tag = "B") +
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
  
p4 <- df_dat %>% 
    ggplot(aes(EFconsPerCap, gini, group = country)) +
    geom_point(size = 0.5) +
    geom_path(size = 0.2, aes(color = year)) +
    annotate(geom = "rect", xmin = 0, xmax = 1.68, ymin = 20, ymax = 30,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom ="rect",xmin = 0, xmax = 1.68, ymin = 30, ymax = 62,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect", xmin = 1.68, xmax = 18, ymin = 20, ymax = 30,
             fill = "orange", alpha = 0.2) +
    annotate(geom="rect", xmin = 1.68, xmax = 18, ymin = 30, ymax = 62,
             fill = "red", alpha = 0.2) +
    geom_hline(yintercept = 30, linetype = 2, color = "gray50") +
    geom_vline(xintercept = 1.68, linetype = 2, color = "gray50") +
    scico::scale_color_scico("Year", palette = "bilbao") +
    labs(y = "Inequality (Gini)", x = "Ecological footprint", tag = "E") + theme_light() +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm"))

p5<- df_dat %>% 
    ggplot(aes(gni2, gini, group = country)) +
    geom_point(size = 0.5, show.legend = FALSE) +
    geom_path(size = 0.2, aes(color = year)) +
    annotate(geom = "rect", xmin = 10, xmax = 12746, ymin = 20, ymax = 30,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect",xmin = 10, xmax = 12746, ymin = 30, ymax = 62,
             fill = "red", alpha = 0.2) +
    annotate(geom ="rect", xmin = 12746, xmax = 105000, ymin = 20, ymax = 30,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom="rect", xmin = 12746, xmax = 105000, ymin = 30, ymax = 62,
             fill = "orange", alpha = 0.2) +
    geom_hline(yintercept = 30, linetype = 2, color = "gray50") +
    geom_vline(xintercept = 12746, linetype = 2, color = "gray50") +
    scico::scale_color_scico("Year", palette = "bilbao") +
    scale_x_log10() +
    labs(y = "Inequality (Gini)", x = "Prosperity (GNI [log])", tag = "F") + theme_light() +
    theme(legend.position = "top", panel.grid = element_blank(),
          legend.key.width = unit(10, "mm"), legend.key.height = unit(3,"mm"))


p6<- df_dat %>% 
    ggplot(aes(gni2, EFconsPerCap, group = country)) +
    geom_point(size = 0.5, show.legend = FALSE) +
    geom_path(size = 0.2, aes(color = year))+
    annotate(geom = "rect", xmin = 10, xmax = 12746, ymin = 0, ymax = 1.68,
             fill = "orange", alpha = 0.2) +
    annotate(geom ="rect",xmin = 10, xmax = 12746, ymin = 1.68, ymax = 18,
             fill = "red", alpha = 0.2) +
    annotate(geom ="rect", xmin = 12746, xmax = 105000, ymin = 0, ymax = 1.68,
             fill = "skyblue", alpha = 0.2) +
    annotate(geom="rect", xmin = 12746, xmax = 105000, ymin = 1.68, ymax = 18,
             fill = "orange", alpha = 0.2) +
    geom_hline(yintercept = 1.68, linetype = 2, color = "gray50") +
    geom_vline(xintercept = 12746, linetype = 2, color = "gray50") +
    scico::scale_color_scico("Year", palette = "bilbao") +
    scale_x_log10() +
    labs(y = "Ecological footprint", x = "Prosperity (GNI [log])", tag = "G") + theme_light() +
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

## Clustering