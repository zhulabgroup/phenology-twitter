#' @export
plot_ideology_structure <- function(p_subgroup_venn, p_ideology_density, p_ideo_cond_prob_temperature, p_ideo_cond_prob_climate, save = F) {
  p <- p_subgroup_venn +
    plot_ideology_arrow(option = 1) +
    p_ideology_density +
    plot_ideology_arrow(option = 2) +
    p_ideo_cond_prob_temperature +
    p_ideo_cond_prob_climate +
    plot_layout(design = "
                ABCDE
                ABCDF
                ") +
    plot_annotation(tag_levels = list(c("a", "", "b", "", "c", "d"))) +
    plot_layout(widths = c(0.8, 0.2, 0.5, 0.2, 0.6))

  if (save) {
    ggsave(
      plot = p,
      filename = "alldata/output/figures/main/ideology_structure.png",
      width = 12,
      height = 5,
      device = png,
      type = "cairo"
    )
  }

  return(p)
}
