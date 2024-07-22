### This script plots the woredas that have reached the WFP trigger 
### For OND 2024

# libraries
library(tidyverse)
library(sf)
library(gghdx)
gghdx()

# reading in admin boundaries
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024")

eth_adm1_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm1_csa_bofedb_2024")

# reading in files
somali_region_woredas <- read_csv(file.path(
  Sys.getenv("CERF_AA_DIR"),
  "Ethiopia", "2024", "Potential Activation", 
  "AA Dashboard - Somali Region, OND Season.csv"))

oromia_region_woredas <- read_csv(file.path(
  Sys.getenv("CERF_AA_DIR"),
  "Ethiopia", "2024", "Potential Activation", 
  "AA Dashboard - Southern Oromia Region, OND Season.csv"))

# plotting woredas where moderate trigger is reached
ond_woredas_triggered <- somali_region_woredas |>
  mutate(Region = "Somali") |>
  bind_rows(oromia_region_woredas |> mutate(Region = "Oromia"))

# moderate areas triggered
ond_woredas_triggered |>
  filter(Severity == "Moderate") |>
  merge(eth_adm3_codab, by.x = "Woreda", by.y = "admin3Name_en", all.y = T) |>
  ggplot() +
  geom_sf(aes(fill = `Triggered?`, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Yes" = "tomato"), na.translate = FALSE) +
  labs(title = "Moderate Drought Trigger Status for OND 2024 in Ethiopia",
       subtitle = "Based on July Forecast with a 3-Month Lead Time")
  
# severe areas triggered
ond_woredas_triggered |>
  filter(Severity == "Severe") |>
  merge(eth_adm3_codab, by.x = "Woreda", by.y = "admin3Name_en", all.y = T) |>
  ggplot() +
  geom_sf(aes(fill = `Triggered?`, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Yes" = "tomato"), na.translate = FALSE) +
  labs(title = "Severe Drought Trigger Status for OND 2024 in Ethiopia",
       subtitle = "Based on July Forecast with a 3-Month Lead Time")
