rink_overlay <- function(p_object){
  p = p_object +
  geom_circle(
  data = data.frame(x0 = 169, y0 = 20.5, r = 15),
  aes(x0 = x0, y0 = y0, r = r),
  lwd = 0.5,
  col = "gray50",
  inherit.aes = FALSE
) + #plot rink on top for better visual
  geom_circle(
    data = data.frame(x0 = 169, y0 = 64.5, r = 15),
    aes(x0 = x0, y0 = y0, r = r),
    lwd = 0.5,
    col = "gray50",
    inherit.aes = FALSE
  ) +
  geom_point(
    inherit.aes = FALSE,
    aes(y = 20.5, x = 169),
    col = "gray50",
    size = 1
  ) +
  geom_point(
    inherit.aes = FALSE,
    aes(y = 64.5, x = 169),
    col = "gray50",
    size = 1
  ) +
  annotate(
    "segment",
    col = "blue",
    x = 125,
    xend = 125,
    y = 0,
    yend = 85,
    lwd = 0.5
  ) +
  geom_segment(
    col = "indianred",
    inherit.aes = FALSE,
    lwd = 0.5,
    aes(
      y = 5.75,
      x = 189,
      yend = 79.25,
      xend = 189
    )
  ) +
  geom_segment(
    col = "indianred",
    inherit.aes = FALSE,
    lwd = 0.5,
    aes(
      y = 39.5,
      x = 192.5,
      yend = 45.5,
      xend = 192.5
    )
  ) +
  geom_segment(
    col = "indianred",
    inherit.aes = FALSE,
    lwd = 0.5,
    aes(
      y = 39.5,
      x = 192.5,
      yend = 39.5,
      xend = 189
    )
  ) +
  geom_segment(
    col = "indianred",
    inherit.aes = FALSE,
    lwd = 0.5,
    aes(
      y = 45.5,
      x = 192.5,
      yend = 45.5,
      xend = 189
    )
  ) +
  geom_path(
    data = upper_outline,
    aes(x = x, y = y),
    colour = "gray50",
    inherit.aes = FALSE,
    lwd = 0.7
  ) +
    annotate("segment", x = 189, xend = 189, y = 39.5, yend = 45.5, linetype = "dashed", color='blue')+ #goal line
    annotate("segment", x = 189, xend = 169, y = 45.5, yend = 64.5, linetype = "dashed", color='blue')+ #goal to Top Faceoff dot
    annotate("segment", x = 169, xend = 154, y = 64.5, yend = 64.5, linetype = "dashed", color='blue')+ #Top Faceoff dot to top of Faceoff Circle
    annotate("segment", x = 154, xend = 154, y = 64.5, yend = 20.5, linetype = "dashed", color='blue')+ #Top of top Faceoff Circle to Top of bottom Faceoff Circle
    annotate("segment", x = 154, xend = 169, y = 20.5, yend = 20.5, linetype = "dashed", color='blue')+ #Top of Bottom Faceoff Circle to Bottom Faceoff Dot
    annotate("segment", x = 169, xend = 189, y = 20.5, yend = 39.5, linetype = "dashed", color='blue')+ #Bottom Faceoff Dot to goal
    annotate("segment", x  =125, xend = 200, y = 42.5, yend = 42.5, linetype = "dashed", color='#22ded0')+ #midline
    theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks  = element_blank(),
    axis.text = element_blank(),
    panel.grid= element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.margin = unit(c(1, 0, 0, 0), "lines")
  )
  return(p)
}
