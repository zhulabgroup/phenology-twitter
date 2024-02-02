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
        group = misc_subset(forlabel = T, oneline = T) %>%
          factor(levels = misc_subset(forlabel = T, oneline = T)),
        message = c(
          "this pollen is killing me",
          "pollen level is high and\nwarm weather will make it worse",
          "global warming could make\nyour pollen allergies a lot worse"
        ),
        x = c(9, 9, 9),
        y = c(4, 2, 0)
      ),
      aes(x = x, y = y, label = str_c(group, ":\ne.g., \"", message, "\""), col = group),
      hjust = 1
    ) +
    scale_color_manual(values = c(
      "dark green",
      "dark blue",
      "dark orange"
    )) +
    coord_fixed(ratio = 2) +
    xlim(-3, 9) +
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

#' @export
misc_subset <- function(forlabel = F, oneline = F) {
  if (!forlabel) {
    v_group <- c("pollen", "pollen-temperature", "pollen-climate")
  }

  if (forlabel) {
    if (!oneline) {
      v_group <- c("discuss\npollen phenology", "attribute to\ntemperature change", "attribute to\nclimate change")
    } else {
      v_group <- c("Discuss pollen phenology", "Attribute to temperature change", "Attribute to climate change")
    }
  }
  return(v_group)
}

misc_subset_n <- function(group, n) {
  group_n <- group %>% 
    str_replace_all(" ", "~") %>% 
    str_c( "~'('~italic(n)~'='~", n, "~')'")
  
  return(group_n)
}
