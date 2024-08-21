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

# file path for writing csv files
eth_ecmwf_woreda_path <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public", "exploration", "eth", "ecmwf")

# read in Ethiopia CODAB
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024")

eth_adm1_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg"), 
  layer = "eth_admbnda_adm1_csa_bofedb_2024")

# copied this from the drought app
trigger_proposal <- read_excel(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "exploration", "eth", "ecmwf", "trigger_proposal.xlsx"), 
  sheet = "Latest")

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
# read in July/August Forecast
july_forecast <- terra::rast(
  paste0("s3://", Sys.getenv("BUCKET_NAME"), 
         "/ECMWF_COGS/202407-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
)

aug_forecast <- terra::rast(
  paste0("s3://", Sys.getenv("BUCKET_NAME"), 
         "/ECMWF_COGS/202408-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
)

# aggregate the july forecast for Ethiopia at admin 3
july_forecast_crop <- crop(july_forecast, raster::extent(eth_adm3_codab))
july_forecast_adm3 <- exact_extract(july_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = c("admin3Pcode", "admin2Pcode"))

# aggregate the july forecast for Ethiopia at admin 3
aug_forecast_crop <- crop(aug_forecast, raster::extent(eth_adm3_codab))
aug_forecast_adm3 <- exact_extract(aug_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = c("admin3Pcode", "admin2Pcode"))

# filter MARS data for OND released in July
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 7 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))
  

# get the lowest 20% value
compute_lower20_seasonal <- function(adm3_forecast, pub_month_filter = 7, valid_months = c(10, 11, 12)) {
  adm3_forecast |>
    dplyr::filter(pub_month == pub_month_filter & valid_month %in% valid_months) |>
    mutate(year = year(valid_date)) |>
    group_by(adm3_pcode, year) |>
    summarise(seasonal_total = sum(value, na.rm = TRUE)) |>
    group_by(adm3_pcode) |>
    summarise(woreda_lower20 = quantile(seasonal_total, 0.2, na.rm = TRUE))
}
ond_forecast_lower20_july <- compute_lower20_seasonal(adm3_forecast, pub_month_filter = 7)
ond_forecast_lower20_aug <- compute_lower20_seasonal(adm3_forecast, pub_month_filter = 8)

# wfp trigger mechanism: 1-in-5 year RP for every month
compute_lower20_monthly <- function(adm3_forecast, pub_month_filter, valid_months = c(10, 11, 12)) {
  adm3_forecast |>
    dplyr::filter(pub_month == pub_month_filter & valid_month %in% valid_months) |>
    mutate(year = year(valid_date)) |>
    group_by(adm3_pcode, year, valid_month) |>
    summarise(monthly_total = sum(value, na.rm = TRUE)) |>
    group_by(adm3_pcode, valid_month) |>
    summarise(woreda_lower20 = quantile(monthly_total, 0.2, na.rm = TRUE)) |>
    mutate(month = month.name[valid_month])
}

### checking if the trigger is below the threshold for each month

# append these results to one object and get the overall season performance
compare_monthly_forecast <- function(forecast_adm3, lower20_monthly, ond_zones) {
  forecast_adm3 |>
    rename_with(~ paste0(month.name[c(7:12, 1)]), starts_with("median")) |>
    pivot_longer(cols = month.name[c(7:12, 1)], names_to = "month", values_to = "monthly_forecast") |> 
    filter(month %in% month.name[c(10:12)]) |>
    merge(lower20_monthly, by.x = c("admin3Pcode", "month"), by.y = c("adm3_pcode", "month")) |>
    mutate(
      ond_inseason = if_else(admin2Pcode %in% ond_zones, TRUE, FALSE), 
      ond_check = if_else(ond_inseason, woreda_lower20, NA_real_),
      ond_below20 = case_when(
        monthly_forecast > ond_check ~ "Above",
        monthly_forecast <= ond_check ~ "Below",
        .default = NA_character_
      )
    )
}
compare_seasonal_forecast <- function(forecast_adm3, lower20_monthly, ond_zones, mon) {
  month_idx <- (mon:(mon + 6)) %% 12
  month_idx[month_idx == 0] <- 12
  
  forecast_adm3 |>
    rename_with(~ paste0(month.name[month_idx]), starts_with("median")) |>
    pivot_longer(cols = month.name[month_idx], names_to = "month", values_to = "monthly_forecast") |> 
    filter(month %in% month.name[c(10:12)]) |>
    group_by(admin3Pcode, admin2Pcode) |>
    summarise(ond_forecast = sum(monthly_forecast, na.rm = TRUE)) |>
    merge(lower20_monthly, by.x = "admin3Pcode", by.y = "adm3_pcode") |>
    mutate(
      ond_inseason = if_else(admin2Pcode %in% ond_zones, TRUE, FALSE), 
      ond_check = if_else(ond_inseason, woreda_lower20, NA_real_),
      ond_below20 = case_when(
        ond_forecast > ond_check ~ "Above",
        ond_forecast <= ond_check ~ "Below",
        .default = NA_character_
      )
    )
}
july_comparison <- compare_seasonal_forecast(july_forecast_adm3, ond_forecast_lower20_july, ond_zones, mon = 7)
aug_comparison <- compare_seasonal_forecast(aug_forecast_adm3, ond_forecast_lower20_aug, ond_zones, mon = 8)

## getting the overall for the season
# July
july_df_plot <- july_comparison |>
  group_by(admin3Pcode, admin2Pcode) |>
  summarise(season_total = sum(ond_forecast, na.rm = T)) |>
  merge(ond_forecast_lower20_july, by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F),
         below20_check = case_when(
           ond_inseason & season_total > woreda_lower20 ~ "Not reached 1-in-5 RP",
           ond_inseason & season_total <= woreda_lower20 ~ "Reached 1-in-5 RP",
           .default = "Out of Season")) |>   
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T)

write_csv(july_df_plot |>
            st_drop_geometry() |>
            dplyr::select(admin3Name_en, admin2Name_en, admin1Name_en, below20_check), 
          file.path(eth_ecmwf_woreda_path, "july_forecast.csv"))

july_df_plot |>
  ggplot() +
  geom_sf(aes(fill = below20_check, geometry = geom)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Not reached 1-in-5 RP" = "steelblue", 
                               "Reached 1-in-5 RP" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "July ECMWF Forecast for OND 2024",
       subtitle = "July forecast vs 1-in-5 year event",
       fill = "")

# August
aug_df_plot <- aug_comparison |>
  group_by(admin3Pcode, admin2Pcode) |>
  summarise(season_total = sum(ond_forecast, na.rm = T)) |>
  merge(ond_forecast_lower20_aug, by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F),
         below20_check = case_when(
           ond_inseason & season_total > woreda_lower20 ~ "Not reached 1-in-5 RP",
           ond_inseason & season_total <= woreda_lower20 ~ "Reached 1-in-5 RP",
           .default = "Out of Season")) |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) 

write_csv(aug_df_plot |>
            st_drop_geometry() |>
            dplyr::select(admin3Name_en, admin2Name_en, admin1Name_en, below20_check), 
          file.path(eth_ecmwf_woreda_path, "august_forecast.csv"))


aug_df_plot |>
  ggplot() +
  geom_sf(aes(fill = below20_check, geometry = geom)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Not reached 1-in-5 RP" = "steelblue", 
                               "Reached 1-in-5 RP" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "August ECMWF Forecast for OND 2024",
       subtitle = "August forecast vs 1-in-5 year event",
       fill = "")

# would the trigger have activated with the triggers
trigger_check <- july_comparison |>
  group_by(admin3Pcode) |>
  summarise(season_total = sum(monthly_forecast, na.rm = T)) |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  merge(trigger_proposal, by.x = "admin3Name_en", by.y = "Woreda", all.x = T) |>
  rename(july_trigger = `...8`) |>
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F),
         trigger_check = case_when(
           ond_inseason & season_total > july_trigger ~ "Not Reached",
           ond_inseason & season_total <= july_trigger ~ "Reached",
           .default = NA))

trigger_check |>
  ggplot() +
  geom_sf(aes(fill = trigger_check, geometry = geom)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Not Reached" = "steelblue", "Reached" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "July Forecast for OND",
       subtitle = "Checking if 1-in-5 year return period trigger would have been reached using July-Aug-Sep-Oct forecasts",
       fill = "July Trigger")

trigger_test <- trigger_check |>
  group_by(trigger_check) |>
  summarise(count = n()) |>
  na.omit() |>
  mutate(percentage = count / sum(count) * 100)

trigger_status <- trigger_test$percentage[trigger_test$trigger_check == "Reached"]

print(paste0("The trigger would not have been reached since the forecast indicates ", 
             round(trigger_status, 2), 
             "% of the OND receiving area will have rainfall below or equal to the trigger."))

# testing if ensemble mean would be below the 20%
july_comparison |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  ggplot() +
  geom_sf(aes(fill = ond_below20, geometry = geom)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Above" = "steelblue", "Below" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "July Forecast for OND",
       subtitle = "July forecast ensemble mean versus 20th percentile of the historical ensemble mean",
       fill = "") +
  facet_wrap(vars(factor(month, levels = month.name)))

ond_below20_test <- july_comparison |>
  group_by(ond_below20, month) |>
  summarise(count = n()) |>
  na.omit() |>
  group_by(month) |>
  mutate(percentage = count / sum(count) * 100)

ond_below20_test |>
  rowwise() |>
  filter(ond_below20 == "Below") |>
  mutate(trigger_status = paste("The forecast indicates that for", month,
                                 round(percentage, 2),
                                 "% of the OND receiving area will have rainfall below or equal to the historic 20%.")) |>
  pull(trigger_status) |>
  walk(print)


ond_below20_test_season <- seasonal_below20 |>
  group_by(below20_check) |>
  summarise(count = n()) |>
  na.omit() |>
  mutate(percentage = count / sum(count) * 100)

ond_below20_test_season |>
  rowwise() |>
  filter(below20_check == "Reached 1-in-5 RP") |>
  mutate(trigger_status = paste("The forecast indicates that",
                                round(percentage, 2),
                                "% of the OND receiving area will have rainfall below or equal to the historic 20%.")) |>
  pull(trigger_status) |>
  walk(print)

## It looks like the trigger, if set using the 20% percentile for forecasts released,
## in July would have activated. 
## If setting an overall frequency of activation of 20% across lead times,
## Then the RP would be less frequent. 
## This is looking at 20% at one lead time and would activate if following WFP methodology