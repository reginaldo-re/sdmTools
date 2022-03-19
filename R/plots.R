#' Plot a SDM_area
#'
#' @param x SDM_area object
#' @param ... Additional parameters
#'
#' @export
#' @method plot SDM_area
plot.SDM_area <- function(x, ...){
  assert(
    check_class(x$study_area, "SpatialPolygons"),
    check_class(x$study_area, "SpatialLines")
  )
  x$study_area %>% plot(...)
}

#' @export
area_geomap <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  UseMethod("area_geomap", an_area)
}

#' @export
area_geomap.SDM_area <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  return(
    an_area$study_area %>%
      .sp_area_geomap(
        title,
        subtitle = ifelse(crs_subtitle==T, paste0(crs(an_area$study_area)), ""),
        lat,
        long,
        group,
        colour,
        fill
      )
  )
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

  map_tmp <- ggplot(
    data =  map_data,
    aes_string(
      x = long,
      y = lat,
      group = group
    )
  ) +
    scale_x_continuous(labels = number_format) +
    scale_y_continuous(labels = number_format) +
    coord_equal()


  if (title != ""){
    map_tmp <- map_tmp +
      labs(title=title)
  }

  if (subtitle != ""){
    map_tmp <- map_tmp +
      labs(subtitle = subtitle)
  }

  if (is.na(fill)){
    map_tmp <- map_tmp +
      geom_polygon(colour = colour, fill = NA)
  } else {
    map_tmp <- map_tmp +
      geom_polygon(colour = NA, aes_string(fill = fill))
  }

  return(map_tmp)
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
      subtitle = ifelse(crs_subtitle==T, paste0(crs(an_area$study_area)), ""),
      lat,
      long,
      group,
      colour,
      fill
    )
  if (!(a_gridded_area %>% is.null())){
    geo_map <- geo_map +
      geom_polygon(
        data = sdm_tidy(a_gridded_area),
        aes(x = long, y = lat, group = group),
        colour = "#4d4d4d",
        fill = NA
      )
  }
  return(geo_map)
}
