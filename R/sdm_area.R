utils::globalVariables(c("where",":="))

#' Creates a Study Area
#'
#' @param an_area A sp object or a path to a file, commonly a shapefile or geopackage, representing the area of study.
#' @param name A name do describe the study area.
#' @param epsg_code A valid epsg_code code, for example EPSG:4326.
#' @param a_res A vector containing the resolution of the study area. The format is two numeric values
#' (width and height) according to epsg_code used. So, the numeric values can be express different types of units of
#' measurement, for example deegres or meters.
#' @return An object representing a study area containing a sp object. Occasional topological errors on polygons of the
#' object are corrected. If the epsg_code of the study area is different from the epsg_code passed to the function, the study
#' area is reproject.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#' )
#' new_sdm_area <- sdm_area(SPDF, "Brasil", "EPSG:6933", c(50000, 50000)))
#'
#' class(new_sdm_area)
#' }
#'
sdm_area <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  UseMethod("sdm_area", an_area)
}

#' @export
sdm_area.character <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  checkmate::assert(
    checkmate::check_file_exists(an_area),
    checkmate::check_string(an_area, min.chars = 3)
  )

  if (! an_area %>% fs::is_file()){
    an_area_path <- .find_files(an_area)
    if (an_area_path %>% is.null()){
      stop("A study area file not found!")
    }
    else if (an_area_path %>% length() > 1){
      stop(
        paste(
          "The file name of study area is ambiguous:",
          paste(
            an_area_path,
            sep = ", ",
            collapse = " "
          )
        )
      )
    } else {
      an_area <- an_area_path
    }
  }

  an_area %>%
    rgdal::readOGR(verbose = F) %>%
    .sp_sdm_area(name, epsg_code, a_res) %>%
    return()
}

.is_gridded <- function(an_area){
  if (! an_area %>% is("SpatialPolygonsDataFrame")) {
    return(F)
  }
  res_summ <- an_area %>%
    raster::area() %>%
    summary()

  res_summ["Median"] == res_summ["Mean"] && (an_area@polygons %>% length() > 1) %>%
    return ()
}

.get_resolution <- function(an_area){
  if (an_area %>% .is_gridded()){
    coords <- an_area@polygons %>%
      magrittr::extract2(1) %>%
      slot("Polygons") %>%
      magrittr::extract2(1) %>%
      slot(("coords"))

    abs(coords[1,1] - coords[2,1]) %>%
      rep(2) %>%
      c() %>%
      return()
  }
  else {
    NULL %>%
      return()
  }
}

#' @export
sdm_area.Spatial <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  an_area %>%
    .sp_sdm_area(name, epsg_code, a_res) %>%
    return()
}

.sp_sdm_area <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  checkmate::assert_string(name)
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  area_crs <- suppressWarnings(try(raster::crs(an_area)))
  if(class(area_crs) == "try-error" || area_crs %>% is.na()){
    raster::crs(an_area) <- raster::crs("EPSG:4326")
  }
  #checkmate::assert_class(result_crs, "CRS", .var.name = "an_area@crs")

  checkmate::assert(
    checkmate::check_null(epsg_code),
    checkmate::check_string(epsg_code, min.chars = 6, fixed = "EPSG:"),
    .var.name = "epsg_code"
  )

  new_crs <- NULL
  if (epsg_code %>% is.null()){
    new_crs <- an_area %>%
      raster::crs()
  } else {
    new_crs <- suppressWarnings(
      try(raster::crs(epsg_code))
    )
    checkmate::assert(
      checkmate::check_class(new_crs, "try-error"),
      checkmate::check_class(new_crs, "CRS"),
      checkmate::check_scalar_na(new_crs),
      .var.name = "epsg_code"
    )
  }

  gridded <- an_area %>%
    .is_gridded()

  if (gridded && a_res %>% is.null()){
    a_res <- an_area %>%
      .get_resolution()
  }
  checkmate::assert_numeric(a_res, len = 2, lower = 0.0001)

  an_area <- an_area %>%
    sp::spTransform(new_crs) %>%
    .repair_area()

  sdm_area_tmp <- list(
    name = (name %>% fs::path_file() %>%  fs::path_ext_remove()),
    crs = new_crs,
    epsg_code = epsg_code,
    resolution = a_res,
    gridded = gridded,
    study_area = an_area,
    scenarios = list()
  )

  structure(
    sdm_area_tmp,
    class= "SDM_area"
  ) %>%
    return()
}


.find_files <- function(an_area = NULL){
  checkmate::check_string(an_area, min.chars = 5)
  file_list <- here::here() %>%
    fs::dir_ls(recurse = T, type = "file") %>%
    stringr::str_subset(stringr::fixed(an_area, ignore_case = T))

  if ((file_list %>% length() >= 1)){
    file_list %>%
      return()
  }
  else {
    NULL %>%
      return()
  }
}

.repair_area <- function(an_area = NULL){
  UseMethod(".repair_area", an_area)
}

.repair_area.default <- function(an_area = NULL){
  an_area %>%
    return()
}

.repair_area.SpatialPolygons <- function(an_area = NULL){
  res_crs <- suppressWarnings(
    try(raster::crs(an_area))
  )
  if (res_crs %>% is("try-error") || res_crs %>% is.na()){
    stop("Invalid CRS.")
  }
  suppressWarnings(
    an_area %>%
      rgeos::gBuffer(byid=TRUE, width=0) %>%
      return()
  )
}

.guess_file_name <- function(an_area = NULL){
  paste(
    an_area$name %>% fs::path_file() %>%  fs::path_ext_remove(),
    ifelse(an_area$gridded && ! an_area$name %>% stringr::str_detect(stringr::fixed("_grid")), "_grid", ""),
    an_area$resolution[1],
    ifelse(! an_area$epsg_code %>% is.null() && ! an_area$name %>% stringr::str_detect(stringr::fixed(an_area$epsg_code)), an_area$epsg_code, "")
  ) %>%
    snakecase::to_snake_case() %>%
    paste0(".gpkg") %>%
    return()
}
