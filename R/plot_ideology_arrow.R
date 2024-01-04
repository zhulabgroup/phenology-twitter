plot_ideology_arrow <- function(option = 1) {
  if (option == 1) {
    p_arrow <- ggplot(data.frame(
      group = misc_subset(forlabel = T, oneline = T),
      start = 0,
      end = 1
    )) +
      geom_segment(aes(x = start, y = 0, xend = end, yend = 0, col = group),
        arrow = arrow(
          length = unit(0.3, "npc"),
          type = "closed" # Describes arrow head (open or closed)
        ), size = 2
      ) +
      scale_color_manual(values = c("dark green", "dark blue", "dark orange")) +
      facet_wrap(. ~ group, ncol = 1) +
      guides(col = "none") +
      theme_void() +
      theme(strip.text = element_blank())
  }
  if (option == 2) {
    p_arrow <- ggplot() +
      geom_curve(
        aes(x = 0, y = 4.5, xend = 0, yend = 2),
        arrow = arrow(
          length = unit(0.03, "npc"),
          type = "closed" # Describes arrow head (open or closed)
        ),
        colour = "dark blue",
        size = 1.2,
        angle = 90,
        curvature = -0.5
      ) +
      geom_label(aes(x = 0.2, y = 3), label = "c", col = "dark blue") +
      geom_curve(
        aes(x = 0, y = 4.5, xend = 0, yend = -0.5),
        arrow = arrow(
          length = unit(0.03, "npc"),
          type = "closed" # Describes arrow head (open or closed)
        ),
        colour = "dark orange",
        size = 1.2,
        angle = 90,
        curvature = -0.5
      ) +
      geom_label(aes(x = 0.5, y = 2), label = "d", col = "dark orange") +
      xlim(0, 1) +
      ylim(-1, 5) +
      theme_void()
  }

  return(p_arrow)
}
