

#' r_proj_file_paths
#' @description
#' convenience function to centralize and standardize r paths and there sourcing within project.
#' 
#' @return named list of file paths needed for project and accessed through R
#'
#' @examples \dontrun{
#' fps <- r_proj_file_paths()
#' }

r_proj_file_paths <-  function(){
  
  list(
    CODAB_FP = file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public","raw","eth","cod_ab",
  "Admin_2024.gdb", "Admin_2024.gdb"
),

  GDB_ECMWF_MARS_TIFS = file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "eth",
  "ecmwf_seasonal",
  "seas51",
  "mars"
)
)
}
