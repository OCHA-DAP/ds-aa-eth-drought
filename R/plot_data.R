

plot_yearly_impact <- function(df_sel) {
  affected_zones <- str_split(df_sel$affected_zones, ", ")[[1]]
  gdf_all_zones <- gdf_pin %>%
    mutate(status = case_when(
      admin2Pcode %in% affected_zones ~ "Zones in lower quartile of seasonal rainfall",
      admin2Pcode %in% sel_zones ~ "Potential zones in season",
      TRUE ~ "Other zones"
    ))
  plot <- ggplot() +
    geom_sf(data = gdf_all_zones, aes(fill = status), color = "black") +
    labs(title = glue("Zones experiencing lower {SEVERITY} seasonal rainfall"),
         fill = "Zone Status") +
    scale_fill_manual(
      values = c("white", "grey", hdx_hex("tomato-hdx")),
      labels = c("Out of season", "In season", glue("Lower {SEVERITY} of seasonal rainfall")),
      name = ""
    )
  return(plot)
}

