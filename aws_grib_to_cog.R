library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(glue)
library(aws.s3)


source("R/cloud_storage_tools.R")
source("R/grib_tools.R")


run_date <- Sys.Date()
run_mo <- format(run_date, "%Y-%m")


# returns a nicely formatted bucket
bucket_df <- get_formatted_bucket_df(
  bucket = Sys.getenv("BUCKET_NAME")
)

# get the bigger files w/ the names of current month included
bucket_df_filt <- bucket_df %>%
  filter(size_mb > 12) %>%
  filter(str_detect(date, run_mo))



# Load Gribs --------------------------------------------------------------


# loop through gribs, and read with rast()
td <- file.path(tempdir(), "ecmwf_gribs")

lr <- map2(
  bucket_df_filt$Key, bucket_df_filt$filename,
  \(keytmp, fntmp){
    fn <- file.path(
      td,
      fntmp
    )

    save_object(bucket = Sys.getenv("BUCKET_NAME"), object = keytmp, file = fn, overwrite = T)
    r <- rast(fn)
    return(r)
  }
)


# Handle Grib MetaData ----------------------------------------------------

grib_files <- list.files(file.path(td, "ecmwf_gribs"), full.names = T)
grib_files_filt <- str_subset(grib_files, pattern = run_mo)
lr <- load_mf_ensemble_mean(file_paths = grib_files_filt)
r <- rast(lr)




# Write Rasters to Storage ------------------------------------------------

td_cog <- file.path(tempdir(), "ecmwf_cogs")
pub_date <- floor_date(Sys.Date(), "month")
prefix_cog_name <- format(pub_date, "%Y%m")
cog_name <- paste0(prefix_cog_name, "-ECMWF_SEAS-V_i_a-ensemble_mean.tif")


# this doesn't work, I believe due to GDAL (at least my version) handling of temp paths
tf_cog <- file.path(td_cog, cog_name)

# therefore I will just write to working directory and delete

# write that to COG
terra::writeRaster(
  r,
  filename = cog_name,
  # filename = tf_cog, # someone else can see if this works, but not for me
  filetype = "COG",
  gdal = c(
    "COMPRESS=DEFLATE",
    "SPARSE_OK=YES",
    "OVERVIEW_RESAMPLING=AVERAGE"
  ), overwrite = T
)


## AWS Bucket ####
aws.s3::put_object(
  file = cog_name,
  object = file.path(
    "ECMWF_COGS",
    cog_name
  ),
  bucket = Sys.getenv("BUCKET_NAME")
)




## Azure Blob ####
es <- azure_endpoint_url()
# storage endpoint
se <- AzureStor::storage_endpoint(es, sas = Sys.getenv("DSCI_AZ_SAS_DEV"))
sc_global <- AzureStor::storage_container(se, "global")


AzureStor::upload_blob(
  container = sc_global,
  src = cog_name,
  dest = paste0("raster/cogs/",cog_name)
)

unlink(cog_name)
unlink(paste0(cog_name, ".aux.json"))