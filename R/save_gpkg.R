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
#'    file_info()
#' }
save_gpkg <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  assert_string(new_name, min.chars = 1, null.ok = T)
  assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("save_gpkg", an_area)
}

#' @export
save_gpkg.SDM_area <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  if (!new_name %>% is.null()){
    new_name <- new_name %>%
      path_ext_remove()

    an_area$sdm_area_name <- new_name %>%
      path_ext_remove()
  }
  if (!an_area$dir_path %>% dir_exists()){
    an_area$dir_path %>%
      dir_create()
  }

  an_area$dir_path %>%
    dir_ls(type = "file") %>%
    file_delete()

  if (dir_path %>% is.null() || dir_path == an_area$dir_path){
    an_area$study_area %>%
      save_gpkg(
        new_name = an_area$sdm_area_name,
        dir_path = an_area$dir_path
      )

    # .sp_save_gpkg(
    #   an_area = an_area$study_area,
    #   new_name = an_area$sdm_area_name,
    #   dir_path = an_area$dir_path,
    #   crs = an_area$study_area %>% crs()
    # )
  } else {
    an_area$study_area %>%
      save_gpkg(
        new_name = an_area$sdm_area_name,
        dir_path = dir_path
      )

    # an_area$study_area %>%
    #   .sp_save_gpkg(
    #     new_name = an_area$sdm_area_name,
    #     dir_path = dir_path,
    #     crs = an_area$study_area %>% crs()
    #   )
  }

  if(!an_area$scenarios %>% is.null()){
    an_area$dir_path %>%
      dir_ls(type = "directory") %>%
      discard(~ . == dir_path %>% path("scenarios")) %>%
      dir_delete()

    tmp_scenario <- an_area$scenarios$dir_path %>%
      path(an_area$scenarios$sdm_scenario_name) %>%
      sdm_scenario()

    if (an_area$scenarios %>% compare(tmp_scenario) %>% length() > 0){
      "The content of dir " %>%
        paste0(an_area$scenarios$dir_path, ". is corrupted!") %>%
        abort()
    }

    if (an_area$scenarios$dir_path %>% path(an_area$scenarios$sdm_scenario_name) != dir_path %>% path(an_area$scenarios$sdm_scenario_name)){
      an_area$scenarios$dir_path %>%
        path(an_area$scenarios$sdm_scenario_name) %>%
        dir_copy(dir_path %>% path(an_area$scenarios$sdm_scenario_name), overwrite = T)
    }

    an_area$scenarios <- dir_path %>%
      path("scenarios") %>%
      sdm_scenario()
  }

  if (!dir_path %>% is.null()){
    an_area$dir_path <- dir_path
  }

  return(an_area)
}

#' @export
save_gpkg.Spatial <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  assert_string(new_name, min.chars = 1)
  assert_string(dir_path, min.chars = 1)
  if (!new_name %>% is.null()){
    new_name <- new_name %>%
      path_ext_remove()
  }


  if (!dir_path %>% dir_exists()){
    quiet(
      dir_path %>%
        dir_create()
    )
  }
  assert_directory_exists(dir_path)

  return(an_area)
}

#' @export
save_gpkg.SpatialLines <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  an_area <- NextMethod(an_area)

  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- 1:(an_area@lines %>% length()) %>%
    as.data.frame() %>%
    rename(id=".")

  .sp_save_gpkg(
    an_area = an_area %>% as("SpatialLinesDataFrame"),
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% crs()
  )

  return(an_area)
}

#' @export
save_gpkg.SpatialLinesDataFrame <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  an_area <- an_area %>%
    save_gpkg.Spatial(new_name, dir_path)

  .sp_save_gpkg(
    an_area = an_area,
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% crs()
  )

  return(an_area)
}

#' @export
save_gpkg.SpatialPolygons <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  an_area <- NextMethod(an_area)

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- 1:(an_area@polygons %>% length()) %>%
    as.data.frame() %>%
    rename(id=".")

  .sp_save_gpkg(
    an_area = an_area %>% as("SpatialPolygonsDataFrame"),
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% crs()
  )

  return(an_area)
}

#' @export
save_gpkg.SpatialPolygonsDataFrame <- function(an_area = NULL, new_name = NULL, dir_path = NULL){
  an_area <- an_area %>%
    save_gpkg.Spatial(new_name, dir_path)

  .sp_save_gpkg(
    an_area = an_area,
    new_name = new_name,
    dir_path = dir_path,
    crs = an_area %>% crs()
  )

  return(an_area)
}


.sp_save_gpkg <- function(an_area = NULL, new_name = NULL, dir_path = NULL, crs = NULL){
  assert(
    check_class(an_area, "SpatialPolygonsDataFrame"),
    check_class(an_area, "SpatialLinesDataFrame")
  )
  assert_string(new_name, min.chars = 1)
  assert_directory_exists(dir_path)
  assert(
    check_class(crs, "CRS"),
    check_null(crs)
  )
  if ("geom" %>% is_in(an_area %>% names())){
    "A variable name of an_area can not be 'geom'. 'geom' is a reserved word, please change the name of that variable." %>%
      abort()
  }
  if (crs %>% is.null()){
    crs <- an_area %>%
      crs()
  }
  if (!an_area %>% compareCRS(crs)){
    an_area <- an_area %>%
      spTransform(crs)
  }

  dsn = ifelse(
    new_name %>% path_ext() == "",
    dir_path %>% path(new_name) %>% paste0(".gpkg"),
    dir_path %>% path(new_name) %>% path_ext_remove() %>% paste0(".gpkg")
  )
  layer = new_name %>%
    path_file() %>%
    path_ext_remove()

  result = quiet(
    an_area %>% writeOGR(
      dsn = dsn,
      layer = layer,
      driver="GPKG",
      overwrite_layer = T
    )
  )
  if (result %>% class() == "try-error"){
    result %>%
      abort()
  }
  return(an_area)
}
