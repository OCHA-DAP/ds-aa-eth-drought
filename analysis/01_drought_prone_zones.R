# frequency of drought

library(tidyverse)
library(scales)
library(raster)
library(sf)
library(gghdx)
gghdx()

rain_data <- read_csv("https://data.humdata.org/dataset/423143be-315f-48d7-9e90-ae23738da564/resource/e8bb806b-c4a4-4892-aa0c-68bb7e490004/download/eth-rainfall-adm2-full.csv")
jjas_woredas <- read_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/jjas_woredas_fewsnet.csv")
mam_ond_woredas <- read_csv("G:/Shared drives/Predictive Analytics/CERF Anticipatory Action/Ethiopia/2024/Trigger Development/mam_ond_woredas_fewsnet.csv")
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
kiremt_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 80 & d_month %in% c(8, 9)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year)) %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T)


# identifying drought prone areas for Belg
belg_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 80 & d_month %in% c(4, 5)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year)) %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T)

# identifying drought prone areas for OND
ond_yr_number <- 43
ond_drought_prone_zones <- rain_data[-1,] %>%
  mutate(across(starts_with("r"), as.numeric), 
         d_year = year(date),
         d_month = month(date)) %>%
  filter(r3q <= 80 & d_month %in% c(12)) %>%
  group_by(ADM2_PCODE) %>%
  summarize(distinct_years = n_distinct(d_year),
            perc_drought_prone = distinct_years / ond_yr_number) %>%
  merge(eth_adm2_codab, by.x = "ADM2_PCODE", by.y = "admin2Pcode", all.y = T) %>%
  
ggplot(data = ond_drought_prone_zones) +
  geom_sf(aes(fill = distinct_years, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  labs(title = "Number of Drought Years by Zone for OND",
       fill = "Number of Years")
