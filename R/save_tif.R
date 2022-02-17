#' Save the vector format geospatial object of the \code{SDM_area} to a raster tif file.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#'
#' @param file_name The name of file which the \code{sp} object will be saved.
#' The \code{file_name} will be prepended to each attribute name of the \code{sp} object.
#' If the \code{file_name} parameter is equal to NULL, it is replaced by
#' a name computed based on attributes of the \code{SDM_area} object.
#' @param file_path The path in which the file will be saved.
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
#'    save_tif(file_path = tmp_dir)
#' }
save_tif <- function(an_area, file_name, file_path){
  UseMethod("save_tif", an_area)
}

#' @export
save_tif.SDM_area <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  if (file_name %>% is.null()) {
    file_name <- .guess_file_name(an_area)
  }
  .sp_save_tif(an_area, file_name, file_path)
}

.sp_save_tif <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  checkmate::assert_class(an_area, "SDM_area")
  checkmate::assert_string(file_path)
  checkmate::assert_string(file_name)

  tmp_raster <- raster::raster(
    crs  = an_area$study_area %>% raster::crs(),
    ext = an_area$study_area %>% raster::extent(),
    res = an_area$resolution
  )

  an_area$study_area@data <- an_area$study_area@data %>%
    dplyr::select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% tidyselect::any_of()))

  tmp_raster <- an_area$study_area %>%
    terra::rasterize(tmp_raster) %>%
    raster::deratify()


  result = tryCatch({
    clear_dir <- file_path %>%
      fs::dir_exists() %>%
      magrittr::not()

    file_path %>%
      fs::path(file_name) %>%
      fs::dir_create(recurse = T)

    tmp_raster %>%
      raster::writeRaster(
        filename = file_path %>% fs::path(file_name) %>% fs::path(names(an_area$study_area)),
        overwrite = T,
        bylayer = T,
        format = "GTiff"
      )
  }, error = function(e){
    e$message %>% print()
    "Error saving file:" %>%
      paste0(fs::path(file_path, file_name)) %>%
      paste0(".")

    if (clear_dir){
      file_path %>%
        fs::dir_delete()
    }
  })
}

