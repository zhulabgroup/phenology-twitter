plot_phenology_compare <- function(p_ts_compare, p_st_compare, p_metrics_compare, save = F) {
  p <- p_ts_compare$line + 
    p_st_compare +
    p_metrics_compare$gradient + 
    plot_layout(design = "
                AAC
                BBB
                BBB
                ")+
    plot_annotation(tag_levels = 'a',
                    theme = theme(plot.tag = element_text(face = "bold")))
    
    if(save) {
      ggsave(
        plot = p,
        filename = "alldata/output/figures/main/compare_phenology.png",
        width = 12,
        height = 12,
        device = png,
        type = "cairo"
      )
    }
  
  return(p)
}
