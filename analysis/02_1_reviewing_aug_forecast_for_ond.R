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

# check the available files
bucket_l <- get_bucket(paste0("s3://", Sys.getenv("BUCKET_NAME")))
bucket_names <- map_chr(.x = bucket_l, .f = \(x) x$Key) |> unname()
bucket_names[str_detect(bucket_names, "ECMWF_COGS")]

# read in August Forecast
aug_forecast <- terra::rast(
  paste0("s3://", Sys.getenv("BUCKET_NAME"), 
         "/ECMWF_COGS/202408-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
)

# filter MARS data for OND released in July
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 8 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))
  

# get the lowest 20% value
ond_forecast_lower20 <- adm3_forecast |>
  dplyr::filter(pub_month == 8 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_lower20 = quantile(seasonal_total, 0.2, na.rm = T))

# wfp trigger mechanism: 1-in-5 year RP for every month
ond_forecast_lower20_monthly <- adm3_forecast |>
  dplyr::filter(pub_month == 8 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year, valid_month) |>
  summarise(monthly_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode, valid_month) |>
  summarise(woreda_lower20 = quantile(monthly_total, 0.2, na.rm = T)) |>
  mutate(month = month.name[valid_month])

# aggregate the july forecast for Ethiopia at admin 3
aug_forecast_crop <- crop(aug_forecast, raster::extent(eth_adm3_codab))
aug_forecast_adm3 <- exact_extract(aug_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = c("admin3Pcode", "admin2Pcode"))


### checking if the trigger is below the threshold for each month

# append these results to one object and get the overall season performance
aug_comparison <- aug_forecast_adm3 |>
  rename_with(~ paste0(month.name[c(8:12, 1:2)]),
              starts_with("median")) |>
  pivot_longer(cols = month.name[c(8:12, 1:2)], 
               names_to = "month",
               values_to = "monthly_forecast") |> 
  filter(month %in% month.name[c(10:12)]) |>
  merge(ond_forecast_lower20_monthly, by.x = c("admin3Pcode", "month"), 
        by.y = c("adm3_pcode", "month")) |>
  # Check if value if below 20th percentile
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F), 
         ond_check = if_else(ond_inseason, woreda_lower20, NA),
         ond_below20 = case_when(monthly_forecast > ond_check ~ "Above",
                                 monthly_forecast <= ond_check ~ "Below",
                                 .default = NA))


# would the trigger have activated with the triggers
trigger_check <- aug_comparison |>
  group_by(admin3Pcode) |>
  summarise(season_total = sum(monthly_forecast, na.rm = T)) |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  merge(trigger_proposal, by.x = "admin3Name_en", by.y = "Woreda", all.x = T) |>
  rename(aug_trigger = `...7`) |>
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F),
         trigger_check = case_when(
           ond_inseason & season_total > aug_trigger ~ "Not Reached",
           ond_inseason & season_total <= aug_trigger ~ "Reached",
           .default = NA))

# output CSV file for trigger data
trigger_csv <- trigger_check |> 
  filter(
    !is.na(trigger_check)
  ) |> 
  select(
    starts_with("admin"),
    `Season total` = season_total,
    `August trigger` = aug_trigger,
    `Trigger check` = trigger_check
  )

trigger_check |>
  ggplot() +
  geom_sf(aes(fill = trigger_check, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Not Reached" = "steelblue", "Reached" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "August Forecast for OND",
       subtitle = "Checking if 1-in-5 year return period trigger would have been reached using July-Aug-Sep-Oct forecasts",
       fill = "August Trigger")

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
aug_comparison |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  ggplot() +
  geom_sf(aes(fill = ond_below20, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Above" = "steelblue", "Below" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "August Forecast for OND",
       subtitle = "August forecast ensemble mean versus 20th percentile of the historical ensemble mean",
       fill = "") +
  facet_wrap(vars(factor(month, levels = month.name)))

ond_below20_test <- aug_comparison |>
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

## getting the overall for the season
seasonal_below20 <- aug_comparison |>
  group_by(admin3Pcode, admin2Pcode) |>
  summarise(season_total = sum(monthly_forecast, na.rm = T)) |>
  merge(ond_forecast_lower20, by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  mutate(ond_inseason = if_else(admin2Pcode %in% ond_zones, T, F),
         below20_check = case_when(
           ond_inseason & season_total > woreda_lower20 ~ "Above",
           ond_inseason & season_total <= woreda_lower20 ~ "Below",
           .default = NA))

# add this to the trigger CSV for the final export values
final_csv <- left_join(
  x = trigger_csv,
  y = select(
    seasonal_below20,
    admin3Pcode,
    `Historical 20%` = woreda_lower20,
    `Below historical 20%` = below20_check
  ),
  by = "admin3Pcode"
)

final_csv |>
  write_csv(
    "eth_aug2024_ecmwf_ond.csv"
  )

seasonal_below20 |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  ggplot() +
  geom_sf(aes(fill = below20_check, geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Above" = "steelblue", "Below" = "tomato"), 
                    na.value = "lightgrey") +
  labs(title = "August Forecast for OND",
       subtitle = "August forecast ensemble mean versus 20th percentile of the historical ensemble mean",
       fill = "")

ond_below20_test_season <- seasonal_below20 |>
  group_by(below20_check) |>
  summarise(count = n()) |>
  na.omit() |>
  mutate(percentage = count / sum(count) * 100)

ond_below20_test_season |>
  rowwise() |>
  filter(below20_check == "Below") |>
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