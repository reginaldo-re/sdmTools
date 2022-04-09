#' Save the vector format geospatial object of the \code{SDM_area} to a raster tif file.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#'
#' @param sdm_area_name The name of file which the \code{sp} object will be saved.
#' The \code{sdm_area_name} will be prepended to each attribute name of the \code{sp} object.
#' If the \code{sdm_area_name} parameter is equal to NULL, it is replaced by
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
#'  make_grid(var_names = list(), sdm_area_name = T)
#'
#' tmp_dir <- tempdir()
#'
#' gridded_area %>%
#'    save_tif(dir_path = tmp_dir)
#' }
save_tif <- function(an_area = NULL, sdm_area_name = NULL, dir_path = NULL){
  assert_string(sdm_area_name, min.chars = 1, null.ok = T)
  assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("save_tif", an_area)
}


#' @export
save_tif.SDM_area <- function(an_area = NULL, sdm_area_name = NULL, dir_path = NULL){
  if (!sdm_area_name %>% is.null()){
    sdm_area_name <- sdm_area_name %>%
      path_ext_remove()

    an_area$sdm_area_name <- sdm_area_name
  }

  if (dir_path %>% is.null()){
    tmp_dir_path <- tempdir() %>%
      path(stri_rand_strings(1,6))
    quiet(
      tmp_dir_path %>%
        path(an_area$sdm_area_name) %>%
        dir_create()
    )
    assert_directory_exists(tmp_dir_path)
    .sp_save_tif(
      an_area = an_area$study_area,
      sdm_area_name = an_area$sdm_area_name,
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

    an_area$dir_path %>%
      assert_directory_exists(
        msg = "A problem occurs on the directory creation (dir_path). A modeling area (an_area) must have a valid directory (dir_path) where data will be saved."
      )
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
      sdm_area_name = an_area$sdm_area_name,
      dir_path = an_area$dir_path,
      crs = an_area$study_area %>% crs(),
      resolution = an_area$resolution
    )
  }
  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_save_tif <- function(an_area = NULL, sdm_area_name = NULL, dir_path = NULL, crs = NULL, resolution = NULL){
  an_area %>%
    assert_class(
      classes = "SpatialPolygons",
      msg = "A modeling area (an_area) must be an object of SpatialPolygons* class."
    )

  sdm_area_name %>%
    assert_string(
      min.chars = 1,
      msg = "A modeling area (an_area) must have a name (sdm_area_name)."
    )
  dir_path %>%
    path(sdm_area_name) %>%
    assert_directory_exists(
      msg = "A problem occurs on the directory creation (dir_path %>% path(sdm_area_name)). A modeling area (an_area) must have a valid directory (dir_path) where data will be saved."
    )
  crs %>%
    assert_class(
      classes = "CRS",
      msg = "A modeling area (an_area) must have a valid CRS."
    )
  resolution %>%
    assert_number(
      lower = 0.0001,
      msg = "A modeling area (an_area) must have a resolution (resolution) expressed according to the EPSG code of the area."
    )

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
        filename = dir_path %>% path(sdm_area_name) %>% path(names(an_area)),
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

