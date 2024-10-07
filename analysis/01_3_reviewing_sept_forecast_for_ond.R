# This script looks at the September Forecast for OND 2024 in Ethiopia
# It checks if the forecast is in the bottom 20%.

# libraries
library(tidyverse)
library(AzureStor)
library(sf)
library(terra)
library(arrow)
library(readxl)
library(exactextractr)
library(gghdx)
gghdx()

# read in Ethiopia CODAB
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

# these are not decided on and change a lot but this is the list I came up with 
# to be defined later
ond_zones <- c("ET0508", "ET0806", "ET0808", "ET0411", "ET0412", "ET0810", "ET0511", 
               "ET0807", "ET0507", "ET0421", "ET0410", "ET0504", "ET0502", "ET0802", 
               "ET0414", "ET0503", "ET0809", "ET0505", "ET0509", "ET0510", "ET0506", 
               "ET0812", "ET0415", "ET0422")

# read in MARS data at woreda level 
adm3_forecast <- read_parquet(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "processed", "eth", "ecmwf_mars", "df_eth_mars_zonal_adm3.parquet")
)

# reading in files
# having some issues with accessing the data on prod via R
# adding this here for now while I figure out what the issue is
sept_lt1 <- rast("data/precip_em_i2024-09-01_lt1.tif")
sept_lt2 <- rast("data/precip_em_i2024-09-01_lt2.tif")
sept_lt3 <- rast("data/precip_em_i2024-09-01_lt3.tif")

# aggregating data at the woreda level
sep_forecast_adm3 <- eth_adm3_codab[c("admin3Pcode", "admin3Name_en", "admin2Pcode")]
sep_forecast_adm3["October"] <- exact_extract(sept_lt1, eth_adm3_codab, "median") * 31
sep_forecast_adm3["November"] <- exact_extract(sept_lt2, eth_adm3_codab, "median") * 30
sep_forecast_adm3["December"] <- exact_extract(sept_lt3, eth_adm3_codab, "median") * 31
ggplot(data = sep_forecast_adm3) + geom_sf(aes(fill = October))

# filter MARS data for OND released in September
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 9 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))
  
# get the lowest 20% value
ond_forecast_lower20 <- adm3_forecast |>
  dplyr::filter(pub_month == 9 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_lower20 = quantile(seasonal_total, 0.2, na.rm = T))

# comparing this against the forecast
### checking if the forecast is in the historical lowest 20%

ond_combined_df <- sep_forecast_adm3 |>
  merge(ond_forecast_lower20,
        by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  rowwise() |>
  mutate(ond_forecast = if_else(admin2Pcode %in% ond_zones, 
                                sum(October, November, December, na.rm = TRUE), 
                                NA_real_),
         drought_condition = if_else(is.na(ond_forecast), NA, 
                                     if_else(ond_forecast <= woreda_lower20 & 
                                               admin2Pcode %in% ond_zones, TRUE, FALSE)),
         return_period = if_else(is.na(drought_condition), NA_character_, 
                                 if_else(drought_condition, 
                                         "1-in-5 Year Return Period Reached", 
                                         "1-in-5 Year Return Period Not Reached"))) 
  
ggplot(data = ond_combined_df |>
         filter(!is.na(return_period))) + 
  geom_sf(aes(fill = return_period)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("1-in-5 Year Return Period Reached" = "tomato", 
                               "1-in-5 Year Return Period Not Reached" = "steelblue"), 
                    na.value = "lightgrey",
                    limits = c("1-in-5 Year Return Period Reached", 
                               "1-in-5 Year Return Period Not Reached")) +
  labs(title = "September Forecast for Oct-Dec 2024",
       subtitle = "Drought Severity",
       fill = "Severity") 

# write csv file
ond_combined_df |>
  filter(!is.na(return_period)) |>
  st_drop_geometry() |>
  select(admin3Pcode, admin3Name_en, admin2Pcode, return_period) |>
  rename(severity = return_period) |>
  write_csv("data/september_forecast_ond_2024.csv")

