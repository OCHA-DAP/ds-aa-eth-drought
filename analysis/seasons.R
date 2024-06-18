eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024")

pheno_vec <- c("asap_mask_crop_v03.tif", "asap_mask_rangeland_v03.tif")

process_rasters <- pheno_vec %>%
  set_names() %>%
  map(\(pheno_obj) {
    pheno_raster <- raster(
      file.path(
        Sys.getenv("AA_DATA_DIR"),
        "public", "raw", "glb", "asap", 
        "reference_data", pheno_obj))
    eth_season_crop <- crop(pheno_raster, extent(eth_adm3_codab))
    eth_season_mask <- mask(eth_season_crop, eth_adm3_codab)
    eth_season_mask[eth_season_mask > 108] <- NA
    eth_adm3_seasons <- raster::extract(eth_season_mask, eth_adm3_codab, fun=median, na.rm = T)[,1]
    return(round(eth_adm3_seasons))
  }) 

eth_adm3_codab %>%
  bind_cols(process_rasters %>% 
              bind_cols()) %>%
  mutate(crop_mask = asap_mask_crop_v03.tif/2,
         rangeland_mask = asap_mask_rangeland_v03.tif/2,
         land_mask = case_when(crop_mask>rangeland_mask | is.na(rangeland_mask) ~ "Cropland",
                               rangeland_mask>crop_mask | is.na(crop_mask) ~ "Rangeland",
                               crop_mask==rangeland_mask ~ "Cropland/Rangelands",
                               .default = NA)) %>%
  st_drop_geometry() %>%
  write_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/woreda_land_mask.csv")

adm3_season_totals <- ldf$adm3 %>%
    rename_with(tolower, everything()) %>%
    mutate(valid_month = month(time),
           month_name = factor(month.abb[valid_month], levels = month.abb)) %>%
    slice(rep(row_number(), each = 3)) %>%
    mutate(season_name = get_seasons(month(ldf$adm3$time))) %>%
    group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en, season_name) %>%
    summarise(total_value = sum(median*1000, na.rm = TRUE)) %>%
    group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en) %>%
    mutate(season_thresh = quantile(total_value, probs = 0.7), 
           in_season = ifelse(total_value >= season_thresh, "In-Seas", ""),
           season_name = factor(season_name, levels = names(season_def))) %>%
    arrange(season_name)

adm3_season_totals %>%
  group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en, season_name) %>%
  summarise(adm3_inseason = sum(in_season == "In-Seas") / n()) %>%
  dplyr::select(season_name, adm3_inseason) %>% 
  filter(adm3_inseason == 1) %>%
  group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en) %>%
  summarise(seasons = paste(season_name, collapse = "-")) %>%
  write_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/seasons_for_woredas_rainfall.csv")


adm3_monthly_means %>%
  group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en, month_name) %>%
  summarise(adm3_inseason = sum(in_season == "In-Seas") / n()) %>%
  dplyr::select(month_name, adm3_inseason) %>% 
  filter(adm3_inseason == 1) %>%
  group_by(admin1pcode, admin1name_en, admin2pcode, admin2name_en, admin3pcode, admin3name_en) %>%
  summarise(concat_months = paste(month_name, collapse = "-")) %>%
  rename(`months in season` = concat_months) %>%
  write_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/seasons_for_woredas_rainfall.csv")
