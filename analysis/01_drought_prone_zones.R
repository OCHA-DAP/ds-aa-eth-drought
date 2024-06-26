# frequency of drought

library(tidyverse)
library(scales)
library(raster)
library(sf)
library(gghdx)
gghdx()

rain_data <- read_csv("https://data.humdata.org/dataset/423143be-315f-48d7-9e90-ae23738da564/resource/e8bb806b-c4a4-4892-aa0c-68bb7e490004/download/eth-rainfall-adm2-full.csv")
jjas_zones <- read_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/jjas_zones_fewsnet.csv")
mam_ond_zones <- read_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/mam_ond_zones_fewsnet.csv")
eth_adm1_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm1_csa_bofedb_2024")
eth_adm2_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm2_csa_bofedb_2024")

# identifying drought prone areas for Kiremt
jjas_yr_number <- 43
jjas_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 85 & d_month %in% c(8, 9)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year),
            perc_drought_prone = distinct_years / jjas_yr_number)  %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T) %>%
  mutate(drought_prone = case_when(
    perc_drought_prone >= 0.3 & ADM2_PCODE %in% jjas_zones$admin2Pcode ~ "Yes",
    ADM2_PCODE %in% jjas_zones$admin2Pcode ~ "No",
    .default = NA))

ggplot(data = jjas_drought_prone_zones) +
  geom_sf(aes(fill = drought_prone, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Yes" = "tomato", "No" = "lightblue"), na.value = "lightgrey") +
  labs(title = "Drought Prone Zones for Kiremt",
       fill = "Drought Prone")


# identifying drought prone areas for Belg
belg_yr_number <- 44
belg_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 80 & d_month %in% c(4, 5)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year),
            perc_drought_prone = distinct_years / belg_yr_number)  %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T) %>%
  mutate(drought_prone = case_when(
    perc_drought_prone >= 0.5 & ADM2_PCODE %in% mam_ond_zones$admin2Pcode ~ "Yes",
    ADM2_PCODE %in% mam_ond_zones$admin2Pcode ~ "No",
    .default = NA))

ggplot(data = belg_drought_prone_zones) +
  geom_sf(aes(fill = drought_prone, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Yes" = "tomato", "No" = "lightblue"), na.value = "lightgrey") +
  labs(title = "Drought Prone Zones for Belg",
       fill = "Drought Prone")

# identifying drought prone areas for OND
ond_yr_number <- 43
ond_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 80 & d_month %in% c(12)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year),
            perc_drought_prone = distinct_years / ond_yr_number)  %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T) %>%
  mutate(drought_prone = case_when(
    perc_drought_prone >= 0.4 & ADM2_PCODE %in% mam_ond_zones$admin2Pcode ~ "Yes",
    ADM2_PCODE %in% mam_ond_zones$admin2Pcode ~ "No",
    .default = NA))

write_csv(ond_drought_prone_zones %>%
            dplyr::select(ADM2_PCODE, admin2Name_en, admin1Name_en, admin1Pcode, drought_prone), 
          "G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/ond_drought_prone_zones.csv")
  
ggplot(data = ond_drought_prone_zones) +
  geom_sf(aes(fill = drought_prone, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Yes" = "tomato", "No" = "lightblue"), na.value = "lightgrey") +
  labs(title = "Drought Prone Zones for OND",
       fill = "Drought Prone")


## seasonal rainfall for OND
rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(date == "2023-12-21") %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T) %>%
  ggplot() +
  geom_sf(aes(fill = r3h_avg, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  labs(title = "Average Seasonal Rainfall for OND",
       fill = "Rainfall (mm)")
