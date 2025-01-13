library(glue)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(repr)
library(scales)
library(stringr)
library(gghdx)
library(httr)
library(tidyr)
library(patchwork)
library(lubridate)
library(terra)
library(tidyterra) 
library(patchwork)
library(RColorBrewer)
library(AzureStor)
library(rlang)
library(ggrepel)

base_url <- "https://imb0chd0prod.blob.core.windows.net"
sas_token <- Sys.getenv("DSCI_AZ_SAS_PROD")
container <- "raster"

gghdx()

CON <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  user = "chdadmin",
  host = "chd-rasterstats-prod.postgres.database.azure.com",
  password = Sys.getenv("AZURE_DB_PW_PROD"),
  port = 5432,
  dbname = "postgres"
)
