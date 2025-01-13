#' Get list of admin2 PCodes for March-April-May and Oct-Nov-Dec seasonal zones in Ethiopia
#' @return Character vector of admin2 PCodes
subset_adm2_ond_mam <- function() {
  df_mam_ond <- read_csv(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public", "exploration", "eth", 
      "mam_ond_zones_fewsnet.csv"
    ),
    show_col_types=FALSE
  )
  ond_zones <- c("ET0508", "ET0806", "ET0808", "ET0411", "ET0412", "ET0810", "ET0511", 
                 "ET0807", "ET0507", "ET0421", "ET0410", "ET0504", "ET0502", "ET0802", 
                 "ET0414", "ET0503", "ET0809", "ET0505", "ET0509", "ET0510", "ET0506", 
                 "ET0812", "ET0415", "ET0422", "ET0408", "ET0417", "ET1600", "ET0811") 

  return(c(df_mam_ond$admin2Pcode, ond_zones))
}


#' Load Ethiopian admin2 boundaries and 2024 Food Security PIN data, joining them into a single sf object
#' @return sf object with admin2 geometries and Food Security PIN data
get_eth_gdf_pin <- function(){
  # Get the admin bounds
  eth_adm2 <- st_read(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
    layer = "eth_admbnda_adm2_csa_bofedb_2024") %>%
    filter(!(admin2Pcode %in% list("ET0000", "ET1000")))
  
  # fname = "Food Security PIN and severity 2025.xlsx"
  # sheet_name <- "WS - 3.1 PIN"

  fname <- "Food Security_PIN_Severity_2024.xlsx"
  sheet_name <- "Cluster PiN"
  
  # Get the PiN data and join with the geodataframe
  df_pin_fs <- read_excel(
    path = file.path(Sys.getenv("AA_DATA_DIR"), "public", "exploration", "eth", "pin", fname),
    skip = 1,  
    col_names = TRUE,
    sheet = sheet_name
  ) %>% 
    group_by(`Admin 2 P-Code`) %>%
    summarise(total_pin = sum(`Cluster PiN`, na.rm = TRUE)) %>%  # or `Food Security Cluster`
    select("Admin 2 P-Code", "total_pin")
  
  gdf_adm2 <- eth_adm2 %>%
    full_join(df_pin_fs, by=c("admin2Pcode"="Admin 2 P-Code"))
  
  return(gdf_adm2)
  
}


#' Query historical rainfall data for specified administrative zones
#' @param table Database table name
#' @param iso3 Three-letter country code 
#' @param adm_level Administrative level
#' @param sel_zones Vector of zone PCodes to filter
#' @return Dataframe of historical rainfall data
get_historical_rainfall <- function(table, iso3, adm_level, sel_zones){
  # Get the historical rainfall data from the database to selected zones
  query <- glue("SELECT * from {table} WHERE iso3='{iso3}' AND adm_level={adm_level}")
  df_precip <- dbGetQuery(CON, query) %>%
    filter(
      pcode %in% sel_zones
    )
  return(df_precip)
}


get_eth_pop <- function() {
  url <- paste0(
    "https://hapi.humdata.org/api/v1/population-social/population?location_code=ETH&admin_level=2&output_format=json&app_identifier=",
    Sys.getenv("HDX_APP_IDENTIFIER"),
    "&limit=10000&offset=0")
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    json_data <- content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(json_data)
    df <- as.data.frame(data$data)
  } else {
    print(paste("Error: ", status_code(response)))
  }
  
  df_pop <- df %>%
    group_by(admin2_code) %>% 
    summarise(
      TotalPop = sum(population, na.rm=TRUE),
    ) %>%
    ungroup()
  
  return(df_pop)
}

get_seas5_stack <- function() {
    pc <- blob_connect$load_proj_containers()
    
    cog_df <- AzureStor$list_blobs(
      container = pc$GLOBAL_CONT,
      dir = "seas5/mars/processed"
    )
    
    container_vp <- paste0("/vsiaz/raster/")
    urls <- paste0(container_vp, cog_df$name)
    
    Sys.setenv(AZURE_STORAGE_SAS_TOKEN=Sys.getenv("DSCI_AZ_SAS_PROD"))
    Sys.setenv(AZURE_STORAGE_ACCOUNT="imb0chd0prod")
    rast(urls)
}





