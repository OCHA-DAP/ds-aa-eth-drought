# Load necessary libraries
library(exactextractr)  # For extracting raster values based on polygons
library(tidyverse)      # For data manipulation and visualization
library(raster)         # For raster data manipulation
library(sf)             # For handling spatial data

data_dir <- Sys.getenv("AA_DATA_DIR")
exploration_dir <- file.path(data_dir, "public", "exploration", "eth")

# Read administrative boundaries for Ethiopia at Admin 3 level
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),    # Get base data directory from environment variable
    "public", "raw", "eth", "cod_ab", "Admin_2024.gpkg"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024"  # Specify layer to read
)

# Define a vector of raster file names for phenological masks
pheno_vec <- c("asap_mask_crop_v03.tif", "asap_mask_rangeland_v03.tif")

# Process each raster file
eth2_process_rasters <- pheno_vec |>
  set_names() |>  # Set names to the raster files for easier access later
  map(\(pheno_obj) {
    # Load the raster data
    pheno_raster <- raster(
      file.path(
        Sys.getenv("AA_DATA_DIR"),  # Get base data directory from environment variable
        "public", "raw", "glb", "asap", 
        "reference_data", pheno_obj))  # Specify path to raster files
    
    # Crop the raster to the extent of the administrative boundaries
    eth_season_crop <- crop(pheno_raster, extent(eth_adm2_codab))
    
    # Mask the raster with administrative boundaries
    eth_season_mask <- mask(eth_season_crop, eth_adm2_codab)
    
    # Set values greater than 200 to NA
    eth_season_mask[eth_season_mask > 200] <- NA
    
    # Extract median values for each administrative area
    eth_adm2_seasons <- raster::extract(eth_season_mask, eth_adm2_codab, fun=median, na.rm = TRUE)[,1]
    
    # Return rounded values
    return(round(eth_adm2_seasons))
  })

# Combine processed rasters with the administrative data
land_mask <- eth_adm2_codab |>
  bind_cols(eth2_process_rasters |> bind_cols()) |>
  
  # Create additional columns for land masks and classifications
  mutate(
    crop_mask = asap_mask_crop_v03.tif / 2,  # Scale crop mask
    rangeland_mask = asap_mask_rangeland_v03.tif / 2,  # Scale rangeland mask
    crop_gt_20 = if_else(crop_mask >= 20, crop_mask, NA_real_),  # Filter crops greater than 20%
    range_gt_20 = if_else(rangeland_mask >= 20, rangeland_mask, NA_real_),  # Filter rangelands greater than 20%
    
    # Classify land types based on the conditions
    land_mask = case_when(
      crop_gt_20 > range_gt_20 | is.na(range_gt_20) ~ "Cropland",
      range_gt_20 > crop_gt_20 | is.na(crop_gt_20) ~ "Rangeland",
      crop_gt_20 == range_gt_20 ~ "Cropland/Rangelands",
      TRUE ~ NA_character_  # Default case
    )
  )

# Export the processed land mask data to a CSV file
land_mask |>
  st_drop_geometry() |>  # Remove geometry for CSV export
  write_csv(file.path(exploration_dir, "zone_land_mask.csv"))

# Visualization of Croplands
ggplot(data = land_mask) +
  geom_sf(aes(fill = crop_gt_20, geometry = geom)) +  # Fill by crop percentage
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +  # Add admin 1 boundaries
  scale_fill_gradient2(low = "#D5FFD5", high = "#3EB489", na.value = "lightgrey") +  # Color gradient
  labs(title = "Croplands (ASAP)", fill = "Percent of Zone covered by Croplands")  # Title and legend

# Visualization of Rangelands
ggplot(data = land_mask) +
  geom_sf(aes(fill = range_gt_20, geometry = geom)) +  # Fill by rangeland percentage
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +  # Add admin 1 boundaries
  scale_fill_gradient2(low = "#FFCDCD", high = "tomato", na.value = "lightgrey") +  # Color gradient
  labs(title = "Rangelands (ASAP)", fill = "Percent of Zone covered by Rangelands")  # Title and legend
