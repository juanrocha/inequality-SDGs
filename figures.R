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

p0 + p3 + p2 + p1 + plot_layout(ncol = 2)

ggsave(
    filename = "conceptual_fig.png",
    path = "figures/", device = "png", 
    width = 4, height = 4, bg = "white", dpi = 400,
    plot = p0 + p3 + p2 + p1 + plot_layout(ncol = 2)
)
  



