# This script looks at the July Forecast for OND in Ethiopia
# It checks if the forecast is below normal.

# libraries
library(aws.s3)
library(tidyverse)
library(sf)
library(raster)
library(arrow)
library(exactextractr)
library(gghdx)
gghdx()

# read in Ethiopia CODAB
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024")

# read in MARS data at woreda level 
adm3_forecast <- read_parquet("../TriggerApp2024/data/df_eth_mars_zonal_adm3.parquet")

# read in July Forecast
july_forecast <- raster(paste0("s3://", Sys.getenv("BUCKET_NAME"), 
                               "/ECMWF_COGS/202407-ECMWF_SEAS-V_i_a-ensemble_mean.tif"))

# filter MARS data for OND released in July
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 7 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))
  
# aggregate the july forecast for Ethiopia at admin 3
july_forecast_crop <- crop(july_forecast, extent(eth_adm3_codab))
july_forecast_adm3 <- exact_extract(july_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = "admin3Pcode")
