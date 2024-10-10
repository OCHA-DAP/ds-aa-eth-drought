# This script analyzes the September Forecast for OND (October-November-December) 2024 in Ethiopia.
# It checks if the forecast for OND is in the bottom 20% of historical data, aggregated to admin 3 level.
# The output is a map of drought severity and a CSV of affected districts.

# Load necessary libraries
library(tidyverse)      # Data manipulation and visualization
library(AzureStor)      # For interacting with Azure storage
library(sf)             # Handling spatial data (simple features)
library(terra)          # Raster data manipulation
library(arrow)          # Reading and writing parquet files
library(readxl)         # Reading Excel files
library(exactextractr)  # Fast raster extraction
library(gghdx)          # Humanitarian Data Exchange theme for ggplot2
gghdx()                 # Activate HDX theme for consistent styling

# Read Ethiopia's admin level 3 (woreda) boundaries from the specified GeoDatabase file
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),  # Retrieve the path to data directory from environment variables
    "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg"),  # File path to GeoDatabase
  layer = "eth_admbnda_adm3_csa_bofedb_2024")  # Layer for admin 3 boundaries

# Read Ethiopia's admin level 1 (regional) boundaries from the same GeoDatabase file
eth_adm1_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg"),
  layer = "eth_admbnda_adm1_csa_bofedb_2024")  # Layer for admin 1 boundaries

# Define OND zones manually, as these are subject to change
# These zones represent the areas to be checked for drought conditions.
ond_zones <- read_csv(file.path(Sys.getenv("AA_DATA_DIR"), 
                      "public", "exploration", "eth", "ond_zones.csv"))$zones

# Read MARS data at woreda (admin 3) level
adm3_forecast <- read_parquet(
  file.path(
    Sys.getenv("AA_DATA_DIR"),  # Retrieve the data directory path from environment variables
    "public", "processed", "eth", "ecmwf_mars", "df_eth_mars_zonal_adm3.parquet")  # Path to the forecast data
)

# Read precipitation forecast data from September for OND 2024
# These files represent three long-term (LT) forecasts for each month (October, November, December)
# Get the SAS token and blob container URL from environment variables
sas_token <- Sys.getenv("DSCI_AZ_SAS_PROD")  # SAS Token stored as an environment variable
blob_url <- Sys.getenv("DSCI_AZ_ENDPOINT_PROD")         # Blob storage URL stored as an environment variable

# Create a Blob endpoint object using the URL and SAS token
blob_endpoint <- blob_endpoint(blob_url, sas = sas_token)

# Specify the container where the forecast files are stored
container <- blob_container(blob_endpoint, "raster")

# List files in the container
file_list <- list_storage_files(container, prefix = "seas5/monthly/processed/")

# List and download the specific forecast files from the blob to local temp files
tif_file1 <- storage_download(container, 
                              "seas5/monthly/processed/precip_em_i2024-09-01_lt1.tif", 
                              dest = "data/precip_em_i2024-09-01_lt1.tif")
tif_file2 <- storage_download(container, 
                              "seas5/monthly/processed/precip_em_i2024-09-01_lt2.tif", 
                              dest = "data/precip_em_i2024-09-01_lt2.tif")
tif_file3 <- storage_download(container, 
                              "seas5/monthly/processed/precip_em_i2024-09-01_lt3.tif", 
                              dest = "data/precip_em_i2024-09-01_lt3.tif")

# Read the TIF files into R as rasters
sept_lt1 <- rast("data/precip_em_i2024-09-01_lt1.tif")  # October forecast
sept_lt2 <- rast("data/precip_em_i2024-09-01_lt2.tif")  # November forecast
sept_lt3 <- rast("data/precip_em_i2024-09-01_lt3.tif")  # December forecast

# Aggregating forecast data to woreda level (admin 3)
# Extract median precipitation forecast values for October, November, and December.
sep_forecast_adm3 <- eth_adm3_codab[c("admin3Pcode", "admin3Name_en", "admin2Pcode")]
sep_forecast_adm3["October"] <- exact_extract(sept_lt1, eth_adm3_codab, "median") * 31  # October forecast
sep_forecast_adm3["November"] <- exact_extract(sept_lt2, eth_adm3_codab, "median") * 30  # November forecast
sep_forecast_adm3["December"] <- exact_extract(sept_lt3, eth_adm3_codab, "median") * 31  # December forecast

# Plot the October forecast on a map for visualization
ggplot(data = sep_forecast_adm3) + geom_sf(aes(fill = October))

# Filter MARS forecast data for OND published in September (pub_month = 9) 
# and select forecasts for October, November, and December (valid_month %in% c(10, 11, 12)).
ond_forecast <- adm3_forecast |>
  dplyr::filter(pub_month == 9 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_mean = mean(seasonal_total, na.rm = T))  # Calculate the seasonal mean

# Get the historical 20th percentile (lowest 20%) of seasonal totals for each woreda (admin 3)
ond_forecast_lower20 <- adm3_forecast |>
  dplyr::filter(pub_month == 9 & valid_month %in% c(10, 11, 12)) |>
  mutate(year = year(valid_date)) |>
  group_by(adm3_pcode, year) |>
  summarise(seasonal_total = sum(value, na.rm = T)) |>
  group_by(adm3_pcode) |>
  summarise(woreda_lower20 = quantile(seasonal_total, 0.2, na.rm = T))  # Calculate 20th percentile

# Merge the forecast data with historical data to compare them
# Also calculate if drought conditions exist by checking if the forecast is in the historical lowest 20%.
ond_combined_df <- sep_forecast_adm3 |>
  merge(ond_forecast_lower20,
        by.x = "admin3Pcode", by.y = "adm3_pcode") |>
  rowwise() |>
  mutate(ond_forecast = if_else(admin2Pcode %in% ond_zones,  # Check if zone is in OND list
                                sum(October, November, December, na.rm = TRUE),  # Sum OND precipitation
                                NA_real_),  # Set NA if not in OND zone
         drought_condition = if_else(is.na(ond_forecast), NA, 
                                     if_else(ond_forecast <= woreda_lower20 & 
                                               admin2Pcode %in% ond_zones, TRUE, FALSE)),  # Drought condition
         return_period = if_else(is.na(drought_condition), NA_character_, 
                                 if_else(drought_condition, 
                                         "1-in-5 Year Return Period Reached",  # Drought condition met
                                         "1-in-5 Year Return Period Not Reached")))  # Drought condition not met

# Plot a map showing districts with drought conditions (1-in-5 year return period reached)
ggplot(data = ond_combined_df |>
         filter(!is.na(return_period))) + 
  geom_sf(aes(fill = return_period)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +  # Add admin 1 boundaries
  scale_fill_manual(values = c("1-in-5 Year Return Period Reached" = "tomato", 
                               "1-in-5 Year Return Period Not Reached" = "steelblue"), 
                    na.value = "lightgrey",
                    limits = c("1-in-5 Year Return Period Reached", 
                               "1-in-5 Year Return Period Not Reached")) +
  labs(title = "September Forecast for Oct-Dec 2024",  # Title of the map
       subtitle = "Drought Severity",  # Subtitle
       fill = "Severity")  # Legend title

# Write the results to a CSV file, containing only rows where a return period is calculated
ond_combined_df |>
  filter(!is.na(return_period)) |>
  st_drop_geometry() |>
  select(admin3Pcode, admin3Name_en, admin2Pcode, return_period) |>
  rename(severity = return_period) |>
  write_csv(file.path(
    Sys.getenv("AA_DATA_DIR"), 
    "public", "processed", "eth", "seas5", "september_forecast_ond_2024.csv"))  # Output CSV file with drought severity info

# Clean up and remove downloaded tif files
file.remove("data/precip_em_i2024-09-01_lt1.tif")
file.remove("data/precip_em_i2024-09-01_lt2.tif")
file.remove("data/precip_em_i2024-09-01_lt3.tif")
