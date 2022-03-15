#' Creates a Study Area
#'
#' A study area (\code{SDm_area}) represents a geographic location delimited by primitive shapes (lines and polygons)  and
#' associated attribute information about characteristics of that location.
#'
#' The attribute information of the study area are constant in time. In contrast, the \code{SDM_area} object
#' has an array attribute storing multiple scenarios, representing attribute information about characteristics of the
#' location that change in time.
#'
#' @param an_area An object of package \code{sp} (<https://cran.r-project.org/web/packages/sp>)
#' or a path to a file, commonly a shapefile or geopackage, representing the area of study.
#' @param name A name do describe the study area.
#' @param epsg_code A valid EPSG code, for example EPSG:4326.
#' @param a_res A vector containing the resolution of the study area. The format is two numeric values
#' (width and height) according to \code{epsg_code} used. So, the numeric values can express different
#' types of measurement units, for example deegres or meters.
#' @return An object representing a study area containing a \code{sp} object.
#'
#' Occasional topological errors on polygons of the object are corrected. As a side effect holes inside
#' polygons are removed.
#'
#' If the \code{epsg_code} of the study area is different from the epsg_code passed to the function, the study
#' area is reproject.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#' )
#' new_sdm_area <- SPDF %>%
#'    sdm_area("Brasil", "EPSG:6933", c(50000, 50000)))
#'
#' class(new_sdm_area)
#'
#' plot(new_sdm_area)
#' }
#'
sdm_area <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL, dir_path = NULL){
  checkmate::assert_string(name, min.chars = 1)
  checkmate::check_string(epsg_code, min.chars = 6, fixed = "EPSG:")
  checkmate::assert_number(a_res, lower = 0.0001)
  checkmate::assert_string(dir_path, min.chars = 1)
  if(dir_path %>% fs::dir_exists()){
    dir_path %>%
      fs::dir_delete()
  }
  dir_path <- quiet(
    dir_path %>%
      fs::dir_create()
  )
  checkmate::assert_directory_exists(dir_path)

  UseMethod("sdm_area", an_area)
}

#' @export
sdm_area.character <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL, dir_path = NULL){
  checkmate::assert_file_exists(an_area)
  new_sdm_area <- an_area %>%
    rgdal::readOGR(verbose = F) %>%
    .sp_sdm_area(
      name = name,
      epsg_code = epsg_code,
      a_res = a_res,
      dir_path = dir_path
    )

  return(new_sdm_area)
}

#' @export
sdm_area.Spatial <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL, dir_path = NULL){
  new_sdm_area <- an_area %>%
    .sp_sdm_area(
      name = name,
      epsg_code = epsg_code,
      a_res = a_res,
      dir_path = dir_path
    )

  return(new_sdm_area)
}

#' @noRd
#' @keywords internal
.sp_sdm_area <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL, dir_path = NULL){
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  checkmate::assert_string(name, min.chars = 1)
  epsg_code <- epsg_code %>% toupper()
  checkmate::check_string(epsg_code, min.chars = 6, fixed = "EPSG:")
  checkmate::assert_number(a_res)
  checkmate::assert_directory_exists(dir_path)

  area_crs <- quiet(
    an_area %>%
      raster::crs()
  )

  if(class(area_crs) == "try-error" || area_crs %>% is.na()){
    raster::crs(an_area) <- raster::crs("EPSG:4326")
  }

  crs_result <- quiet(
    epsg_code %>% raster::crs()
  )
  checkmate::assert_class(crs_result, "CRS", .var.name = "epsg_code")

  is_gridded <- quiet(
      an_area %>%
        .is_gridded()
  )

  if( is_gridded %>% is.na()){
    "An_area object has an invalid CRS!" %>%
      rlang::abort()
  }

  if (is_gridded){
    calculated_res <- an_area %>%
      .get_resolution()
    checkmate::assert_number(calculated_res)
    checkmate::assert_number(a_res, lower = calculated_res, upper = calculated_res)
  }
  an_area <- an_area %>%
    sp::spTransform(crs_result) %>%
    .repair_area()

  sdm_area_tmp <- list(
    name = name,
    crs = crs_result,
    epsg_code = epsg_code,
    resolution = a_res,
    gridded = is_gridded,
    study_area = an_area,
    scenarios = list()
  )


  an_area %>%
    save_gpkg(
      file_name = name %>% snakecase::to_snake_case(),
      file_path = dir_path)


  return(
    structure(
      sdm_area_tmp,
      class= "SDM_area"
    )
  )
}


#' @noRd
#' @keywords internal
.repair_area <- function(an_area = NULL){
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines")
  )

  crs_result <- quiet(
    an_area %>%
      raster::crs()
  )
  checkmate::assert_class(crs_result, "CRS", .var.name = "epsg_code")

  if (an_area %>% class() == "SpatialPolygons"){
    quiet(
      an_area <- an_area %>%
        rgeos::gBuffer(byid = TRUE, width = 0)
    )
  }
  return(an_area)
}

#' @noRd
#' @keywords internal
.guess_file_name <- function(an_area = NULL){
  checkmate::assert_class(an_area, "SDM_area")
  return(
    paste(
      an_area$name %>% fs::path_file() %>%  fs::path_ext_remove(),
      an_area$resolution[1],
      ifelse(
        !an_area$epsg_code %>% is.null() && !an_area$name %>% stringr::str_detect(stringr::fixed(an_area$epsg_code)),
        an_area$epsg_code,
        ""
      )
    ) %>%
      snakecase::to_snake_case()
  )
}

#' @noRd
#' @keywords internal
.is_gridded <- function(an_area){
  checkmate::check_class(an_area, "Spatial")
  if (!an_area %>% is("SpatialPolygonsDataFrame")) {
    return(F)
  }
  res_summ <- an_area %>%
    raster::area() %>%
    summary()

  return(res_summ["Median"] == res_summ["Mean"] && (an_area@polygons %>% length() > 1))
}

#' @noRd
#' @keywords internal
.get_resolution <- function(an_area){
  checkmate::check_class(an_area, "Spatial")
  if (an_area %>% .is_gridded()){
    coords <- an_area@polygons %>%
      magrittr::extract2(1) %>%
      slot("Polygons") %>%
      magrittr::extract2(1) %>%
      slot(("coords"))

    return(abs(coords[1,1] - coords[2,1]))
  }
  else {
    return(NULL)
  }
}
