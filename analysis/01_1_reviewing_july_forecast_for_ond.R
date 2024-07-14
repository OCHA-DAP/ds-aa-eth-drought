# This script looks at the July Forecast for OND in Ethiopia
# It checks if the forecast is below normal.
# Also checks if the tentative triggers have been reached.

# libraries
library(aws.s3)
library(tidyverse)
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

# copied this from the drought app
trigger_proposal <- read_excel(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "exploration", "eth", "ecmwf", "trigger_proposal.xlsx"), sheet = "Latest")

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
# read in July Forecast
july_forecast <- terra::rast(
  paste0("s3://", Sys.getenv("BUCKET_NAME"), 
         "/ECMWF_COGS/202407-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
)

# filter MARS data for OND released in July
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 7 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))
  

# get the lowest 20% value
ond_forecast_lower20 <- adm3_forecast |>
  dplyr::filter(pub_month == 7 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_lower20 = quantile(seasonal_total, 0.2, na.rm = T))

# aggregate the july forecast for Ethiopia at admin 3
july_forecast_crop <- crop(july_forecast, raster::extent(eth_adm3_codab))
july_forecast_adm3 <- exact_extract(july_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = c("admin3Pcode", "admin2Pcode"))


# append these results to one object and get the overall season performance
july_comparison <- july_forecast_adm3 |>
  rowwise() |>
  mutate(season_total = sum(`median.2024-07-01.3`, 
                            `median.2024-07-01.4`, 
                            `median.2024-07-01.5`, na.rm = T)) |>
  merge(ond_forecast_lower20, by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  # Check if value if below 20th percentile
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F), 
         ond_woredas = if_else(admin2Pcode %in% ond_zones, woreda_lower20, NA),
         ond_trigger = case_when(season_total > ond_woredas ~ "Above",
                                 season_total <= ond_woredas ~ "Below",
                                 .default = NA))


# would the trigger have activated with the triggers
trigger_check <- july_comparison |>
  merge(eth_adm3_codab, all.y = T) |>
  merge(trigger_proposal, by.x = "admin3Name_en", by.y = "Woreda", all.x = T) |>
  rename(july_trigger = `...8`) |>
  mutate(trigger_check = case_when(
    ond_inseason & season_total > july_trigger ~ "Not Reached",
    ond_inseason & season_total <= july_trigger ~ "Reached",
    .default = NA))

trigger_check |>
  ggplot() +
  geom_sf(aes(fill = trigger_check, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Not Reached" = "steelblue", "Reached" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "July Forecast for OND",
       subtitle = "Checking if trigger would have been reached",
       fill = "July Trigger")

trigger_test <- trigger_check |>
  group_by(trigger_check) |>
  summarise(count = n()) |>
  mutate(percentage = count / sum(count) * 100) |>
  na.omit()

trigger_status <- trigger_test$percentage[trigger_test$trigger_check == "Reached"]

print(paste0("The trigger would not have been reached since ", 
             round(trigger_status, 2), 
             "% of the OND receiving area would have had more rainfall than the July trigger."))

# testing if ensemble mean would be below the 20%
trigger_check |>
  ggplot() +
  geom_sf(aes(fill = ond_trigger, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Above" = "steelblue", "Below" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "July Forecast for OND",
       subtitle = "July forecast ensemble mean versus 20th percentile of the historical ensemble mean",
       fill = "")
