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

.sp_area_geomap <- function(an_area = NULL, title = "", subtitle = "", lat = "lat", long = "long", group = "group", colour = "black", fill = NA) {
  number_format <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

  if (("cell_id" %in% (an_area %>% names()))){
    map_data <- an_area %>%
      sdm_tidy("cell_id")
  }
  else {
    map_data <- an_area %>%
      sdm_tidy()
  }

  map_tmp <- ggplot2::ggplot(
    data =  map_data,
    ggplot2::aes_string(
      x = long,
      y = lat,
      group = group
    )
  ) +
    ggplot2::scale_x_continuous(labels = number_format) +
    ggplot2::scale_y_continuous(labels = number_format) +
    ggplot2::coord_equal()


  if (title != ""){
    map_tmp <- map_tmp +
      ggplot2::labs(title=title)
  }

  if (subtitle != ""){
    map_tmp <- map_tmp +
      ggplot2::labs(subtitle = subtitle)
  }

  if (is.na(fill)){
    map_tmp <- map_tmp +
      ggplot2::geom_polygon(colour = colour, fill = NA)
  } else {
    map_tmp <- map_tmp +
      ggplot2::geom_polygon(colour = NA, ggplot2::aes_string(fill = fill))
  }

  map_tmp %>%
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
