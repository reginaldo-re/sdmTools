#' Save the vector format geospatial object of the \code{SDM_area} to a raster tif file.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#'
#' @param new_name The name of file which the \code{sp} object will be saved.
#' The \code{new_name} will be prepended to each attribute name of the \code{sp} object.
#' If the \code{new_name} parameter is equal to NULL, it is replaced by
#' a name computed based on attributes of the \code{SDM_area} object.
#' @param dir_path The path in which the file will be saved.
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
#'    save_tif(dir_path = tmp_dir)
#' }
save_tif <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  checkmate::assert_string(new_name, min.chars = 1, null.ok = T)
  checkmate::assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("save_tif", an_area)
}


#' @export
save_tif.SDM_area <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  if (!new_name %>% is.null()){
    an_area$sdm_area_name <- new_name
  }

  if (dir_path %>% is.null()){
    tmp_dir_path <- tempdir() %>%
      fs::path(stringi::stri_rand_strings(1,6))
    quiet(
      tmp_dir_path %>%
        fs::path(an_area$sdm_area_name) %>%
        fs::dir_create()
    )
    checkmate::assert_directory_exists(tmp_dir_path)
    .sp_save_tif(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = tmp_dir_path,
      crs = an_area$study_area %>% raster::crs(),
      resolution = an_area$resolution
    )

    if (an_area$dir_path %>% fs::dir_exists()){
      an_area$dir_path %>%
        fs::dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        fs::path(an_area$sdm_area_name) %>%
        fs::dir_create()
    )
    checkmate::assert_directory_exists(an_area$dir_path)
    tmp_dir_path %>%
      fs::dir_copy(an_area$dir_path, overwrite = T)

    tmp_dir_path %>%
      fs::dir_delete()
  } else {
    an_area$dir_path <- dir_path
    if (an_area$dir_path %>% fs::dir_exists()){
      an_area$dir_path %>%
        fs::dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        fs::path(an_area$sdm_area_name) %>%
        fs::dir_create()
    )
    .sp_save_tif(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = an_area$dir_path,
      crs = an_area$study_area %>% raster::crs(),
      resolution = an_area$resolution
    )
  }
  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_save_tif <- function(an_area = NULL, new_name = NULL, dir_path = NULL, crs = NULL, resolution = NULL){
  checkmate::assert_class(an_area, "SpatialPolygons")
  checkmate::assert_string(new_name, min.chars = 1)
  checkmate::assert_directory_exists(dir_path %>% fs::path(new_name))
  checkmate::assert_class(crs, "CRS")
  checkmate::assert_number(resolution, lower = 0.0001)

  tmp_raster <- raster::raster(
    crs = crs,
    ext = an_area %>% raster::extent(),
    res = resolution
  )

  an_area@data <- an_area@data %>%
    dplyr::select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% tidyselect::any_of()))

  tmp_raster <- an_area %>%
    terra::rasterize(tmp_raster) %>%
    raster::deratify()

  result = quiet(
    tmp_raster %>%
      raster::writeRaster(
        filename = dir_path %>% fs::path(new_name) %>% fs::path(names(an_area)),
        overwrite = T,
        bylayer = T,
        format = "GTiff"
      )
  )
  if (result %>% class() == "try-error"){
    result %>%
      rlang::abort()
  }
}

