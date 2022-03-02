#' Save the vector format geospatial object of the \code{SDM_area} to a geopackage file.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#'
#' @param file_name The name of file which the \code{sp} object will be saved. It
#' can be composed by the file name and the file path. If the file path doesn't exists
#' it will be created. If the file name parameter is equal to NULL, it is replaced by
#' a name computed based on attributes of the \code{SDM_area} object.
#' @param file_path The path in which the file will be saved. If file_name contains
#' file path, the file_path parameter is ignored.
#'
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("vect_files/brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = F
#' )
#'
#' gridded_area <- SPDF %>%
#'  sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
#'  make_grid(var_names = list(), new_name = T)
#'
#' tmp_dir <- tempdir()
#'
#' gridded_area %>%
#'    save_gpkg(file_path = tmp_dir)
#'
#' tmp_dir %>%
#'    paste0("/test_area_grid_50000_epsg_6933.gpkg") %>%
#'    fs::file_info()
#' }
save_gpkg <- function(an_area, file_name, file_path){
  UseMethod("save_gpkg", an_area)
}

#' @export
save_gpkg.SDM_area <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  if (file_name %>% is.null() || file_name == "") {
    file_name <- .guess_file_name(an_area)
  }
  .sp_save_gpkg(an_area$study_area, file_name, file_path)
}


#' @export
save_gpkg.Spatial <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  .sp_save_gpkg(an_area, file_name, file_path)
}

.sp_save_gpkg <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  checkmate::assert_class(an_area, "Spatial")
  checkmate::assert_string(file_name)

  if (file_name %>% fs::path_dir() != "."){
    checkmate::check_null(file_path)
    file_path <- file_name %>% fs::path_dir()
  }
  checkmate::assert_string(file_path)

  file_name <- file_name %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    paste0(".gpkg")

  result = tryCatch({
    clear_dir <- file_path %>%
      fs::dir_exists() %>%
      magrittr::not()

    file_path %>%
      fs::dir_create(recurse = T)

    an_area %>% rgdal::writeOGR(
      dsn = file_path %>% fs::path(file_name),
      layer = file_name %>% fs::path_file() %>% fs::path_ext_remove(),
      driver="GPKG",
      overwrite_layer = T
    )
  }, error = function(e){
    c(
      "Error saving file:",
      paste0(fs::path(file_path, file_name, ".")),
      e$message
      ) %>%
      rlang::abort()

    if (clear_dir){
      file_path %>%
        fs::dir_delete()
    }
  })
}
