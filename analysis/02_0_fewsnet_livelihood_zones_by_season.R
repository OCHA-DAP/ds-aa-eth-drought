# Load necessary libraries
library(tidyverse)
library(scales)
library(raster)
library(sf)
library(gghdx)
gghdx()

# Set up data directory from environment variable
data_dir <- Sys.getenv("AA_DATA_DIR")
exploration_dir <- file.path(Sys.getenv("AA_DATA_DIR"), "public", "exploration", "eth")

# Define file paths for administrative boundaries and livelihood zones
codab_path <- file.path(data_dir, "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg")
lhz_path <- file.path(data_dir, "public", "raw", "eth", "lhz", "ET_LHZ_2018")

# Load administrative boundaries and livelihood zones
eth_adm3_codab <- st_read(codab_path, layer = "eth_admbnda_adm3_csa_bofedb_2024")
eth_adm1_codab <- st_read(codab_path, layer = "eth_admbnda_adm1_csa_bofedb_2024")
eth_adm2_codab <- st_read(codab_path, layer = "eth_admbnda_adm2_csa_bofedb_2024")
eth_adm3_lhz <- st_read(lhz_path, layer = "ET_LHZ_2018")

# Visualize the livelihood zones
ggplot(data = eth_adm3_lhz) +
  geom_sf(aes(fill = LZTYPE)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "FEWS NET Livelihood Zones", fill = "Livelihood Zone")

# Spatial analysis for Admin 3
# Align Coordinate Reference Systems (CRS) if they differ
if (st_crs(eth_adm3_codab) != st_crs(eth_adm3_lhz)) {
  eth_adm3_lhz <- st_transform(eth_adm3_lhz, st_crs(eth_adm3_codab))
}

# Perform spatial join and calculate areas
most_cover <- st_join(eth_adm3_codab, eth_adm3_lhz, join = st_intersects) |>
  mutate(area = st_area(geom)) |>
  group_by(admin3Pcode) |>
  slice_max(order_by = area, with_ties = FALSE) |>
  ungroup()

# Visualize most covered livelihood zones by woreda
ggplot(data = most_cover) +
  geom_sf(aes(fill = LZTYPE)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "FEWS NET Livelihood Zones by Woreda", fill = "Livelihood Zone")

# Save results to a shapefile
st_write(most_cover, file.path(data_dir, "public", "processed", "eth", "lhz", "most_cover_shapefile.gpkg"), append = FALSE)

# Export to CSV
write_csv(most_cover |>
            st_drop_geometry() |>
            dplyr::select(admin3Name_en, admin3Pcode, admin2Name_en, admin2Pcode, admin1Name_en, admin1Pcode, LZTYPE), 
          file.path(exploration_dir, "fewsnet_lhz_woredas.csv"))

# Categorize woredas by season
woredas_fewsnet <- most_cover |>
  mutate(season = case_when(
    (LZTYPE %in% c("Cropping - Belg Dominant", "Pastoral", "Agropastoral") | 
       admin3Name_en %in% c("Dolo Ado", "Bokolmayo", "Nyngatom", "Dasenech /Kuraz")) & 
      admin1Name_en != "Gambela" ~ "MAM/OND",
    LZTYPE %in% c("Cropping - Meher Dominant") | admin1Name_en %in% c("Gambela") ~ "JJAS",
    TRUE ~ NA_character_))

# Filter and save JJAS woredas
jjas_woredas_fewsnet <- woredas_fewsnet |>
  filter(season == "JJAS") |>
  st_drop_geometry() |>
  write_csv(file.path(exploration_dir, "jjas_woredas_fewsnet.csv"))

# Filter and save MAM/OND woredas
mam_ond_woredas_fewsnet <- woredas_fewsnet |>
  filter(season == "MAM/OND") |>
  st_drop_geometry() |>
  write_csv(file.path(exploration_dir, "mam_ond_woredas_fewsnet.csv"))

# Visualize woredas categorized by season
ggplot(data = woredas_fewsnet) +
  geom_sf(aes(fill = season)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "FEWS NET Seasons by Woreda", fill = "Seasons")

# Spatial analysis for Admin 2
# Align CRS if they differ
if (st_crs(eth_adm2_codab) != st_crs(eth_adm3_lhz)) {
  eth_adm3_lhz <- st_transform(eth_adm3_lhz, st_crs(eth_adm2_codab))
}

# Perform spatial join and area calculation for Admin 2
most_cover <- st_join(eth_adm2_codab, eth_adm3_lhz, join = st_intersects) |>
  mutate(area = st_area(geom)) |>
  group_by(admin2Pcode) |>
  slice_max(order_by = area, with_ties = FALSE) |>
  ungroup()

# Visualize most covered livelihood zones by zone
ggplot(data = most_cover) +
  geom_sf(aes(fill = LZTYPE)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "FEWS NET Livelihood Zones by Zone", fill = "Livelihood Zone")

# Export to CSV for Admin 2
write_csv(most_cover |>
            st_drop_geometry() |>
            dplyr::select(admin2Name_en, admin2Pcode, admin1Name_en, admin1Pcode, LZTYPE), 
          file.path(exploration_dir, "fewsnet_lhz_zones.csv"))

# Categorize zones by season
zones_fewsnet <- most_cover |>
  mutate(season = case_when(
    LZTYPE %in% c("Cropping - Meher Dominant") | admin1Name_en %in% c("Gambela") ~ "JJAS",
    LZTYPE %in% c("Cropping - Belg Dominant", "Pastoral", "Agropastoral") ~ "MAM/OND",
    TRUE ~ NA_character_))

# Filter and save JJAS zones
jjas_zones_fewsnet <- zones_fewsnet |>
  filter(season == "JJAS") |>
  st_drop_geometry() |>
  write_csv(file.path(exploration_dir, "jjas_zones_fewsnet.csv"))

# Filter and save MAM/OND zones
mam_ond_zones_fewsnet <- zones_fewsnet |>
  filter(season == "MAM/OND") |>
  st_drop_geometry() |>
  write_csv(file.path(exploration_dir, "mam_ond_zones_fewsnet.csv"))

# Visualize zones categorized by season
ggplot(data = zones_fewsnet) +
  geom_sf(aes(fill = season)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "FEWS NET Seasons by Zone", fill = "Seasons")
