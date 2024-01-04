#' @export
read_subgroup_label <- function(path_coding = "alldata/intermediate/coding/", valid = T) {
  if (!valid) {
    ls_df_group_label <- vector(mode = "list")
    for (group in misc_subset()) {
      ls_df_group_label[[group]] <- read_csv(str_c(path_coding, group, "_labeled.csv")) %>% as_tibble()
    }

    return(ls_df_group_label)
  }


  if (valid) {
    ls_df_group_valid <- vector(mode = "list")
    for (group in misc_subset()) {
      if (group == "pollen") {
        ls_df_group_valid[[group]] <- read_csv(str_c(path_coding, group, "_labeled.csv")) %>%
          filter(
            pollen_phenology == 1
          )
      }
      if (group == "pollen-temperature") {
        ls_df_group_valid[[group]] <- read_csv(str_c(path_coding, group, "_labeled.csv")) %>%
          filter(
            pollen_phenology == 1,
            temperature_change == 1,
            correlation == 1
          )
      }
      if (group == "pollen-climate") {
        ls_df_group_valid[[group]] <- read_csv(str_c(path_coding, group, "_labeled.csv")) %>%
          filter(
            pollen_phenology == 1,
            climate_change == 1,
            causation == 1
          )
      }
    }
    return(ls_df_group_valid)
  }
}
