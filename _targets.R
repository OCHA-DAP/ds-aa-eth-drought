library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble",
               "terra",
               "sf",
               "tidyverse",
               "exactextractr",
               "janitor",
               "arrow",
               "glue") # packages that your targets need to run
)

options(clustermq.scheduler = "multicore")

tar_source()

fps <- r_proj_file_paths()

list(

# Load Inputs -------------------------------------------------------------

## AOI - Admin 3 Level ####

  tar_target(
    name = gdf_adm3,
    command = st_read(fps$CODAB_FP,
                      layer = "eth_admbnda_adm3_csa_bofedb_2024") %>% 
      clean_names() %>% 
      mutate(
        area = st_area(.),
        pct_area = as.numeric(area/sum(area))
      ) %>% 
      dplyr::select(
        matches("^admin\\d|^area|pct_area")
      ) %>%
      rename(adm0_en = admin0name_en, adm0_pcode = admin0pcode,
             adm1_en = admin1name_en, adm1_pcode = admin1pcode,
             adm2_en = admin2name_en, adm2_pcode = admin2pcode,
             adm3_en = admin3name_en, adm3_pcode = admin3pcode)
  ),

##  Raster Tifs ####
  tar_target(
    name = r_ecmwf_mars,
    command = load_mars_raster(gdb = fps$GDB_ECMWF_MARS_TIFS)
  ),


# Zonal Statistics --------------------------------------------------------

## Zonal Means to Admin 3 ####
  tar_target(
    name = df_ecmwf_zonal_adm3,
    command= zonal_ecmwf_mars(
      r_wrapped = r_ecmwf_mars,
      zone = gdf_adm3 ,
      stat = "mean",
      cols_keep = colnames(gdf_adm3)
      )
  ),


# Aggregate from Admin 3 to Rest ------------------------------------------
  tar_target(
    name= ldf_ecmwf_zonal,
    command =map(
      c(adm0="adm0",adm1="adm1",adm2="adm2",adm3="adm3"),
      \(rep_level_tmp){
        aggregate_tabular_forecast(df = df_ecmwf_zonal_adm3,
                                   report_level = rep_level_tmp) 
      }
    )
  )
)