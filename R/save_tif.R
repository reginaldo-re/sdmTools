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
  assert_string(new_name, min.chars = 1, null.ok = T)
  assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("save_tif", an_area)
}


#' @export
save_tif.SDM_area <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  if (!new_name %>% is.null()){
    new_name <- new_name %>%
      path_ext_remove()

    an_area$sdm_area_name <- new_name
  }

  if (dir_path %>% is.null()){
    tmp_dir_path <- tempdir() %>%
      path(stringi::stri_rand_strings(1,6))
    quiet(
      tmp_dir_path %>%
        path(an_area$sdm_area_name) %>%
        dir_create()
    )
    assert_directory_exists(tmp_dir_path)
    .sp_save_tif(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = tmp_dir_path,
      crs = an_area$study_area %>% crs(),
      resolution = an_area$resolution
    )

    if (an_area$dir_path %>% dir_exists()){
      an_area$dir_path %>%
        dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        path(an_area$sdm_area_name) %>%
        dir_create()
    )
    assert_directory_exists(an_area$dir_path)
    tmp_dir_path %>%
      dir_copy(an_area$dir_path, overwrite = T)

    tmp_dir_path %>%
      dir_delete()
  } else {
    an_area$dir_path <- dir_path
    if (an_area$dir_path %>% dir_exists()){
      an_area$dir_path %>%
        dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        path(an_area$sdm_area_name) %>%
        dir_create()
    )
    .sp_save_tif(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = an_area$dir_path,
      crs = an_area$study_area %>% crs(),
      resolution = an_area$resolution
    )
  }
  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_save_tif <- function(an_area = NULL, new_name = NULL, dir_path = NULL, crs = NULL, resolution = NULL){
  assert_class(an_area, "SpatialPolygons")
  assert_string(new_name, min.chars = 1)
  assert_directory_exists(dir_path %>% path(new_name))
  assert_class(crs, "CRS")
  assert_number(resolution, lower = 0.0001)

  tmp_raster <- raster(
    crs = crs,
    ext = an_area %>% extent(),
    res = resolution
  )

  an_area@data <- an_area@data %>%
    select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% any_of()))

  tmp_raster <- an_area %>%
    rasterize(tmp_raster) %>%
    deratify()

  result = quiet(
    tmp_raster %>%
      writeRaster(
        filename = dir_path %>% path(new_name) %>% path(names(an_area)),
        overwrite = T,
        bylayer = T,
        format = "GTiff"
      )
  )
  if (result %>% class() == "try-error"){
    result %>%
      abort()
  }
}

