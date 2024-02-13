# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.


CODAB_FP = file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public","raw","eth","cod_ab",
  "eth_adm_csa_bofedb_2021_shp"
)
# "eth_admbnda_adm0_csa_bofedb_itos_2021" ,
# "eth_admbnda_adm1_csa_bofedb_2021",
# "eth_admbnda_adm2_csa_bofedb_2021",

gdb_ecmwf_mars_tifs <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "eth",
  "ecmwf_seasonal",
  "seas51",
  "mars"
)

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

list(
  tar_target(
    name = gdf_adm3,
    command = st_read(CODAB_FP,
                      layer = "eth_admbnda_adm3_csa_bofedb_2021") %>% 
      clean_names() %>% 
      mutate(
        area = st_area(.),
        pct_area = as.numeric(area/sum(area))
      ) %>% 
      select(
        matches("^adm\\d_[ep]|^area|pct_area")
      )
  ),
  tar_target(
    name = r_ecmwf_mars,
    command = load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
  ),
  tar_target(
    name = df_ecmwf_zonal_adm3,
    command= zonal_ecmwf_mars(
      r_wrapped = r_ecmwf_mars,
      zone = gdf_adm3 ,
      stat = "mean",
      cols_keep = colnames(gdf_adm3)
      )
  ),
  tar_target(
    name= df_ecmwf_zonal,
    command =map(
      c("adm2","adm1","adm0"), \(rep_level_tmp){
        aggregate_tabular_forecast(df = df_ecmwf_zonal_adm3,
                                   report_level = rep_level_tmp) %>% 
          
          # write parquets direct to trigger App for updating
          write_parquet(
            glue("../TriggerApp2024/data/df_eth_mars_zonal_{rep_level_tmp}.parquet")
          )
      }
    )
  )
)