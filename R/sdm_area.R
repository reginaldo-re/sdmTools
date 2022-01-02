utils::globalVariables(c("where"))

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

#' Drop noncontiguous polygons with an area smaller or equal lower_bound.
#'
#' @param an_area A SDM_area object representing the area of study.
#' @param lower_bound A lower bound area indicating polygons which it going to dropped out.
#' @return A SDM_area containing a sp object with remaining disaggregated polygons with area greater than lower_bound.
#' @export
#' @examples
#' \dontrun{
#' main <- cbind(
#'   c(0, 0, 1, 1),
#'   c(0, 1, 1, 0)
#' )
#' secondary <- cbind(
#'   c(1, 1.3, 1.3, 1),
#'   c(1, 1.0, 0.7, 0.7)
#' )
#' hole <- main / 3 + 1 / 3
#' island <- cbind(
#'   c(1.05, 1.05, 1.55, 1.55),
#'   c(0, .5, .5, 0)
#' )
#' P <- Polygons(
#'   ID = 1,
#'   list(
#'     Polygon(main),
#'     Polygon(hole, hole = TRUE),
#'     Polygon(island),
#'     Polygon(secondary)
#'   )
#' )
#'
#' SP <- SpatialPolygons(list(P))
#' new_sdm_area <- sdm_area(SP, "EPSG:6933", c(0.1, 0.1)))
#' plot(new_sdm_area)
#' }
areas_gt <- function(an_area = NULL, lower_bound = 0){
  UseMethod("areas_gt")
}

#' @export
areas_gt.default <- function(an_area = NULL, lower_bound = 0) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
areas_gt.SDM_area <- function(an_area = NULL, lower_bound = 0) {
  checkmate::check_class(an_area$study_area, "SpatialPolygons")
  an_area$study_area <- an_area$study_area %>%
    .sp_areas_gt(lower_bound)

  an_area %>%
    return()
}

#' Make a grid over study area.
#'
#' @param an_area A SDM_area object representing the area of study.
#' @param var_names A list of variable names to keep on cells. Variables area computed using
#' the average of features (polygons or lines) that over each cell. It try to match each variable name
#' (ignoring case) in the study area.
#' @param centroid A boolean indicating if x_centroid and y_centroid variables must be computed and appended
#' to variables./
#' @return A SDM_area  object with cells covering the study area. The dataframe contains the variables
#'  matched and computed acoording to each cell.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#'  )
#' SLDF <- readOGR(
#'    system.file("hydro_uper_prpy.gpkg", package="sdmTools"),
#'    layer = "hydro_uper_prpy",
#'    verbose = FALSE
#'  )
#' new_sdm_area <- sdm_area(SPDF, "EPSG:6933", c(50000, 50000)))
#' gridded_area <- make_grid(
#'       new_sdm_area,
#'       var_names = c("Length", "xxx", "Main_ri"),
#'       centroid = T
#'  )
#' gridded_area %>% plot()
#'
#' new_sdm_area <- sdm_area(SLDF, "EPSG:6933", c(50000, 50000)))
#' gridded_area <- make_grid(
#'       new_sdm_area,
#'       var_names = c("Length", "xxx", "Main_ri"),
#'       centroid = T
#'  )
#' gridded_area %>% plot()
#' }
make_grid <- function(an_area = NULL, var_names = NULL, has_centroid=T){
  UseMethod("make_grid", an_area)
}

#' @export
make_grid.default <- function(an_area = NULL, var_names = NULL, has_centroid=T){
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names=NULL, centroid=T){
  checkmate::assert(
    checkmate::check_class(an_area$study_area, "SpatialPolygons"),
    checkmate::check_class(an_area$study_area, "SpatialLines")
  )
  if (an_area$gridded){
    warning("Nothing to do, the grid over that study area already exists.")

    an_area %>%
      return()
  }

  if (an_area$study_area %>% is("SpatialPolygons")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialPolygons(an_area$resolution, var_names, centroid)
  } else if (an_area$study_area %>% is("SpatialLines")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialLines(an_area$resolution, var_names, centroid)
  }

  an_area$gridded <- T

  an_area %>%
    return()
}


.select_vars <- function(a_df = NULL, var_names = NULL){
  checkmate::assert_data_frame(a_df)
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, unique = T)
  )

  a_df <- a_df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.numeric)
    )

  if (! var_names %>% is.null()){
    var_names <- c("cell_id") %>%
      purrr::prepend(var_names)

    if (var_names %>% length() > 0){
      a_df <- a_df %>%
        dplyr::select(var_names %>% unlist() %>% dplyr::contains())
      #dplyr::rename_all(tolower) %>%
      #dplyr::select(var_names %>% tolower() %>% any_of())

      var_names <- a_df %>%
        names() %>%
        map_chr(~ unlist(var_names[stringr::str_detect(.x, stringr::fixed(var_names %>% unlist(), ignore_case=T))]))

      #var_names <- (var_names %>% unlist())[var_names %>% tolower() %in% (a_df %>% names() %>% tolower())]
      #var_names <- a_df %>% names() %>% map_chr(~ stringr::str_subset(var_names, stringr::fixed(., ignore_case=T)))
      names(a_df) <- var_names
    }
    else {
      a_df <- a_df %>%
        dplyr::select(-everything())
    }
  }
  a_df %>%
    return()
}


#' Plot a SDM_area
#'
#' @param x SDM_area object
#' @param ... Additional parameters
#'
#' @export
#' @method plot SDM_area
plot.SDM_area <- function(x, ...){
  checkmate::assert(
    checkmate::check_class(x$study_area, "SpatialPolygons"),
    checkmate::check_class(x$study_area, "SpatialLines")
  )
  x$study_area %>% plot(...)
}

#' Merge rasters over a gridded study area.
#'
#' @param an_area A SDM_area object with cells covering the study area.
#' @param area_source A path to a folder or a Raster* object with variables to merge with.
#' @param var_names A list of variable names to keep on cells. Variables area computed using
#' the average of raster points that over each cell. It try to match each variable name
#' (ignoring case) in the study area.
#'
#' @return A SDM_area_gridded object containing variables merged with. If the CRS of the Raster* is
#' different from the CRS of the SDM_area object, it is reproject. The merging process
#' produces cells intersecting SDM_area object and Raster*.
#' @export
#'
#' @examples
#' \dontrun{
#' aaa
#' }
merge_area <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL){
  UseMethod("merge_area", an_area)
}

#' @export
merge_area.default <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  an_area %>%
    return()
}

#' @export
merge_area.SDM_area <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL){
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }
  an_area %>%
    .sp_merge_area(to_merge_area, var_names) %>%
    return()
}


#' @export
area_geomap <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  UseMethod("area_geomap", an_area)
}


#' @export
area_geomap.SDM_area <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  an_area$study_area %>%
    .sp_area_geomap(
      title,
      subtitle = ifelse(crs_subtitle==T, paste0(raster::crs(an_area$study_area)), ""),
      lat,
      long,
      group,
      colour,
      fill
    ) %>%
    return()
}



repair_area <- function(an_area = NULL){
  UseMethod("repair_area", an_area)
}

repair_area.default <- function(an_area = NULL){
  an_area %>%
    return()
}


#' @export
grid_geomap <- function(an_area = NULL, a_gridded_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  UseMethod("grid_geomap", an_area)
}


#' @export
grid_geomap.SDM_area <- function(an_area = NULL, a_gridded_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  geo_map <- an_area$study_area %>%
    .sp_area_geomap(
      title,
      subtitle = ifelse(crs_subtitle==T, paste0(raster::crs(an_area$study_area)), ""),
      lat,
      long,
      group,
      colour,
      fill
    )
  if (!(a_gridded_area %>% is.null())){
    geo_map <- geo_map +
        ggplot2::geom_polygon(
            data = sdm_tidy(a_gridded_area),
            ggplot2::aes(x = long, y = lat, group = group),
            colour = "#4d4d4d",
            fill = NA
          )
  }
  geo_map %>%
    return()
}
