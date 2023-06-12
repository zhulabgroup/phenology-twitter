# combine panels
p_pattern <- grid.arrange(
  annotate_figure(p_tw_ts,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  annotate_figure(p_tw_map,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  annotate_figure(p_keyword_cloud,
    fig.lab = "c",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1, 1),
    c(2, 3)
  ),
  widths = c(1, 1),
  heights = c(1, 1)
)

# save figure file
if (.fig_save) {
  ggsave(
    plot = p_pattern,
    filename = str_c(.path$fig, "fig-pattern.png"),
    width = 8,
    height = 5,
    device = png, type = "cairo"
  )
}


# combine panels
p_nab <- grid.arrange(
  annotate_figure(p_ts_comp,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  annotate_figure(p_doy_corr,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1),
    c(2)
  ),
  # widths = c(1, 2),
  heights = c(1, 1)
)

# save figure file
if (.fig_save) {
  ggsave(
    plot = p_nab,
    filename = str_c(.path$fig, "fig-nab.png"),
    width = 8,
    height = 8,
    device = png, type = "cairo"
  )
}


# combine panels
p_ideo <- grid.arrange(
  annotate_figure(p_venn,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  annotate_figure(p_ideo_density,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  annotate_figure(p_ideo_cond1,
    fig.lab = "c",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1, 2),
    c(3, 3)
  ),
  widths = c(0.8, 1),
  heights = c(1, 1.5)
)

# save figure file
if (.fig_save) {
  ggsave(
    plot = p_ideo,
    filename = str_c(.path$fig, "fig-ideo.png"),
    width = 8,
    height = 6,
    device = png, type = "cairo"
  )
}
