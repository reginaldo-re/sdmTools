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
save_gpkg <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  checkmate::assert_string(new_name, min.chars = 1, null.ok = T)
  checkmate::assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("save_gpkg", an_area)
}

#' @export
save_gpkg.SDM_area <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  if (!new_name %>% is.null()){
    an_area$sdm_area_name <- new_name %>%
      fs::path_ext_remove()
  }

  if (dir_path %>% is.null()){
    tmp_dir_path <- tempdir() %>%
      fs::path(stringi::stri_rand_strings(1,6))
    quiet(
      tmp_dir_path %>%
        fs::dir_create()
    )
    checkmate::assert_directory_exists(tmp_dir_path)
    .sp_save_gpkg(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = tmp_dir_path,
      crs = an_area$study_area %>% raster::crs()
    )

    if (an_area$dir_path %>% fs::dir_exists()){
      an_area$dir_path %>%
        fs::dir_delete()
    }
    quiet(
      an_area$dir_path %>%
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
        fs::dir_create()
    )
    .sp_save_gpkg(
      an_area = an_area$study_area,
      new_name = an_area$sdm_area_name,
      dir_path = an_area$dir_path,
      crs = an_area$study_area %>% raster::crs()
    )
  }
  return(an_area)
}

#' @export
save_gpkg.SpatialLines <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  checkmate::assert_string(new_name, min.chars = 1)
  checkmate::assert_string(dir_path, min.chars = 1)
  new_name <- new_name %>%
    fs::path_ext_remove()

  if (!dir_path %>% fs::dir_exists()){
    quiet(
      dir_path %>%
        fs::dir_create()
    )
  }
  checkmate::assert_directory_exists(dir_path)

  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- 1:(an_area@lines %>% length()) %>%
    as.data.frame() %>%
    rename(id=".")

  .sp_save_gpkg(
    an_area = an_area %>% as("SpatialLinesDataFrame"),
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% raster::crs()
  )

  return(an_area)
}

#' @export
save_gpkg.SpatialPolygons <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  checkmate::assert_string(new_name, min.chars = 1)
  checkmate::assert_string(dir_path, min.chars = 1)
  new_name <- new_name %>%
    fs::path_ext_remove()


  if (!dir_path %>% fs::dir_exists()){
    quiet(
      dir_path %>%
        fs::dir_create()
    )
  }
  checkmate::assert_directory_exists(dir_path)

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- 1:(an_area@polygons %>% length()) %>%
    as.data.frame() %>%
    rename(id=".")

  .sp_save_gpkg(
    an_area = an_area %>% as("SpatialPolygonsDataFrame"),
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% raster::crs()
  )

  return(an_area)
}

.sp_save_gpkg <- function(an_area = NULL, new_name = NULL, dir_path = NULL, crs = NULL){
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygonsDataFrame"),
    checkmate::check_class(an_area, "SpatialLinesDataFrame")
  )
  checkmate::assert_string(new_name, min.chars = 1)
  checkmate::assert_directory_exists(dir_path)
  checkmate::assert(
    checkmate::check_class(crs, "CRS"),
    checkmate::check_null(crs)
  )
  if (crs %>% is.null()){
    crs <- an_area %>%
      raster::crs()
  }
  if (!an_area %>% raster::compareCRS(crs)){
    an_area <- an_area %>%
      sp::spTransform(crs)
  }

  result = quiet(
    an_area %>% rgdal::writeOGR(
      dsn = dir_path %>% fs::path(new_name) %>% paste0(".gpkg"),
      layer = new_name %>% fs::path_file() %>% fs::path_ext_remove(),
      driver="GPKG",
      overwrite_layer = T
    )
  )
  if (result %>% class() == "try-error"){
    result %>%
      rlang::abort()
  }
}
