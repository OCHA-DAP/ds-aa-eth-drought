#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint

# adding code to convert targets object to parquet
tar_load(ldf_ecmwf_zonal)

# writing to parquet
names(ldf_ecmwf_zonal) |>
  purrr::map(~ {
    adm_lvl <- .x
    file_name <- paste0("data/df_eth_mars_zonal_", adm_lvl, ".parquet")
    df <- ldf_ecmwf_zonal[[adm_lvl]]
    arrow::write_parquet(df, file_name)
  })

