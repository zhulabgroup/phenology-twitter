#' @export
plot_information_flow <- function(ls_df_group_flow, ls_df_group_user_type, save = F) {
  ls_p_sankey <- vector(mode = "list")
  for (group in misc_subset(forlabel = F)) {
    # Combine both data frames
    df_sankey <- ls_df_group_flow[[group]] %>%
      mutate(interaction = row_number()) %>%
      left_join(ls_df_group_user_type[[group]], by = c("to" = "user")) %>%
      select(-to) %>%
      rename(to = "type") %>%
      left_join(ls_df_group_user_type[[group]], by = c("from" = "user")) %>%
      select(-from) %>%
      rename(from = "type") %>%
      drop_na() %>%
      mutate(freq = count / sum(count)) %>%
      select(-count) %>%
      mutate(from_type = from) %>%
      gather(key = "end", value = "type", -interaction, -freq, -from_type) %>%
      mutate(end = factor(end, levels = c("from", "to") %>% rev(), labels = c("from", "to") %>% rev())) %>%
      mutate(type = factor(type, levels = c("media", "expert", "other\nindividual", "other\norganization") %>% rev())) %>%
      mutate(from_type = factor(from_type, levels = c("media", "expert", "other\nindividual", "other\norganization") %>% rev())) %>%
      mutate(
        type_label1 = case_when(end == "from" ~ type),
        type_label2 = case_when(end == "to" ~ type)
      ) %>%
      select(interaction, everything())

    # df_sankey_sum <- df_sankey %>%
    #   group_by(end, type) %>%
    #   summarise(freq_sum = sum(freq)) %>%
    #   ungroup() %>%
    #   mutate(freq_label = round(freq_sum*100) %>% str_c("%")) %>%
    #   select(end, type, freq_label)

    p <- ggplot(df_sankey, aes(y = end, id = interaction, split = type, value = freq)) +
      ggforce::geom_parallel_sets(aes(fill = from_type), alpha = 0.3, axis.width = 0.1) +
      ggforce::geom_parallel_sets_axes(aes(fill = type), axis.width = 0.1) +
      ggforce::geom_parallel_sets_labels(aes(
        label = after_stat(str_c(label, (value * 100) %>% round() %>% str_c("%"), sep = "\n")),
        col = type_label1
      ), angle = 0, nudge_y = 0.3) +
      ggforce::geom_parallel_sets_labels(aes(
        label = after_stat(str_c(label, (value * 100) %>% round() %>% str_c("%"), sep = "\n")),
        col = type_label2
      ), angle = 0, nudge_y = -0.3) +
      scale_fill_manual(values = c("media" = "dark orange", "expert" = "dark blue", "other\norganization" = "black", "other\nindividual" = "dark green"), na.value = NA) +
      scale_color_manual(values = c("media" = "dark orange", "expert" = "dark blue", "other\norganization" = "black", "other\nindividual" = "dark green"), na.value = NA) +
      theme_void() +
      guides(
        fill = "none",
        col = "none"
      ) +
      theme(axis.text.y = element_text(angle = 0)) +
      theme(strip.text = element_text(size = 12)) +
      scale_x_continuous(expand = expansion(mult = c(.02, .06))) +
      ggtitle(case_when(
        group == misc_subset(forlabel = F)[1] ~ str_c("a. ", misc_subset(forlabel = T, oneline = T)[1]),
        group == misc_subset(forlabel = F)[2] ~ str_c("b. ", misc_subset(forlabel = T, oneline = T)[2]),
        group == misc_subset(forlabel = F)[3] ~ str_c("c. ", misc_subset(forlabel = T, oneline = T)[3])
      ))

    ls_p_sankey[[group]] <- p
  }

  p_sankey <- ls_p_sankey %>%
    wrap_plots(ncol = 1)

  if (save) {
    ggsave(
      plot = p_sankey,
      filename = "alldata/output/figures/main/information_flow.png",
      width = 9,
      height = 9,
      device = png,
      type = "cairo"
    )
  }
  return(p_sankey)
}
