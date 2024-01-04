#' @export
plot_subgroup_venn <- function(save = F) {
  p <- ggplot() +
    ggforce::geom_circle(aes(x0 = 0, y0 = 2, r = 3), fill = "dark green", col = "dark green", alpha = 0.5) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 1, r = 2), fill = "white", col = "white", alpha = 1) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 1, r = 2), fill = "dark blue", col = "dark blue", alpha = 0.5) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), fill = "white", col = "white", alpha = 1) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), fill = "dark orange", col = "dark orange", alpha = 0.5) +
    geom_label(
      data = data.frame(
        group = misc_subset(forlabel = T) %>% factor(levels = misc_subset(forlabel = T)),
        x = c(1.5, 1.5, 1.5),
        y = c(4, 2, 0)
      ),
      aes(x = x, y = y, label = group, col = group)
    ) +
    scale_color_manual(values = c(
      "dark green",
      "dark blue",
      "dark orange"
    )) +
    coord_fixed() +
    guides(col = "none") +
    scale_fill_identity() +
    theme_void()

  if (save) {
    ggsave(
      plot = p,
      filename = "alldata/output/figures/supp/subgroup_venn.png",
      width = 4,
      height = 4,
      device = png,
      type = "cairo"
    )
  }
  return(p)
}

misc_subset <- function(forlabel = F, oneline = F) {
  if (!forlabel) {
    v_group <- c("pollen", "pollen-temperature", "pollen-climate")
  }

  if (forlabel) {
    if (!oneline) {
      v_group <- c("discuss\npollen phenology", "attribute to\ntemperature", "attribute to\nclimate change")
    } else {
      v_group <- c("discuss pollen phenology", "attribute to temperature", "attribute to climate change")
    }
  }
  return(v_group)
}
