load_proj_containers <- function() {
  # storage endpoint
  sdev <- storage_endpoint(azure_endpoint_url(), sas = Sys.getenv("DSCI_AZ_SAS_DEV"))
  sprod <- storage_endpoint(azure_endpoint_url(stage = "prod"), sas = Sys.getenv("DSCI_AZ_SAS_PROD"))
  # storage container
  sc_global <- storage_container(sprod, "raster")
  sc_projects <- storage_container(sdev, "projects")
  list(
    GLOBAL_CONT = sc_global,
    PROJECTS_CONT = sc_projects
  )
}


azure_endpoint_url <- function(
    service = c("blob", "file"),
    stage = c("dev", "prod"),
    storage_account = "imb0chd0") {
  blob_url <- "https://{storage_account}{stage}.{service}.core.windows.net/"
  service <- arg_match(service)
  stage <- arg_match(stage)
  storae_account <- arg_match(storage_account)
  endpoint <- glue(blob_url)
  return(endpoint)
}