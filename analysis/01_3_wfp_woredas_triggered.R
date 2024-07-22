### This script plots the woredas that have reached the WFP trigger 
### For OND 2024

# libraries
library(tidyverse)
library(sf)
library(AzureStor)
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
# i am sure there are better ways of downloading multiple files
blob_endpoint <- storage_endpoint("https://imb0chd0dev.blob.core.windows.net", 
                                  key = Sys.getenv("AZURE_BLOB_KEY1"))
container <- storage_container(blob_endpoint, "projects")
som_temp_file <- tempfile(fileext = ".csv")
somali_csv_blob <- storage_download(container, 
                                    src = "ds-aa-eth-drought/exploration/AA Dashboard - Somali Region, OND Season.csv", 
                                    dest = som_temp_file)
somali_region_woredas <- read_csv(som_temp_file)
oro_temp_file <- tempfile(fileext = ".csv")
oromia_csv_blob <- storage_download(container, 
                                    src = "ds-aa-eth-drought/exploration/AA Dashboard - Southern Oromia Region, OND Season.csv", 
                                    dest = oro_temp_file)
oromia_region_woredas <- read_csv(oro_temp_file)

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

# Combine moderate and severe areas
ond_woredas_triggered |>
  group_by(Woreda) |>
  arrange(desc(Severity)) |> # to ensure severe comes before moderate
  slice(1) |>
  merge(eth_adm3_codab, by.x = "Woreda", by.y = "admin3Name_en", all.y = TRUE) |>
  mutate(Severity = ifelse(is.na(Severity), NA, Severity)) |>
  ggplot() + 
  geom_sf(aes(fill = Severity, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Severe" = "tomato", "Moderate" = "orange"),
                    labels = c("Moderate (1-in-3 year event)", "Severe (1-in-5 year event)"), na.value = "lightgrey", na.translate = FALSE) +
  labs(title = "OND 2024 Forecast for Ethiopia",
       subtitle = "Evaluated Using the July EMI forecast with a 3-Month Lead Time",
       fill = "Drought Severity Category")
