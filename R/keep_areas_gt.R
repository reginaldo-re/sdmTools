#' Drop non-contiguous polygons with area smaller or equal lower_bound.
#'
#' @param an_area A \code{SDM_area} object representing the area of study.
#' @param new_name A name to new area study after dropping smaller polygons.
#' @param lower_bound A lower bound area indicating polygons which it going to dropped out.
#'
#' @return A \code{SDM_area} containing an object of package
#' \code{sp} (<https://cran.r-project.org/web/packages/sp>)
#' with remaining disaggregated polygons with area greater than lower_bound.
#' @export
#' @examples
#' \dontrun{
#' new_SPDF <- SP %>%
#'  as("SpatialPolygonsDataFrame")

#' new_SPDF@data <- list(
#'  area=c(
#'    SP[1] %>% gArea(),
#'    SP[2] %>% gArea(),
#'    SP[3] %>% gArea()
#'  )
#') %>%
#'  as.data.frame()
#'
#' plot(new_SPDF)
#'
#' new_area <- new_SPDF %>%
#'  sdm_area(
#'     "Removing a single area from a SpatialPolygons study area.",
#'     "EPSG:6933",
#'     c(50000, 50000)
#'  ) %>%
#'  keep_areas_gt(
#'     0.25,
#'     new_name = "Removing a single area from a SpatialPolygons study area."
#'  )
#'
#' plot(new_area$study_area)
#' }
keep_areas_gt <- function(an_area = NULL, lower_bound = 0, new_name = NULL, dir_path = NULL){
  checkmate::assert_class(an_area$study_area, "SpatialPolygons")
  checkmate::assert_string(new_name, min.chars = 1, null.ok = T)
  checkmate::assert_string(dir_path, min.chars = 1, null.ok = T)

  if (!new_name %>% is.null()){
    an_area$name <- new_name %>%
      snakecase::to_snake_case()
  }
  if (!dir_path %>% is.null()){
    an_area$dir_path <- an_area$dir_path
  }
  new_name <- an_area$name
  dir_path <- an_area$dir_path

  if(dir_path %>% fs::dir_exists()){
    dir_path %>%
      fs::dir_delete()
  }
  quiet(
    dir_path %>%
      fs::dir_create()
  )
  checkmate::assert_directory_exists(dir_path)

  an_area$study_area <- an_area$study_area %>%
    .sp_keep_areas_gt(lower_bound)

  an_area %>%
    save_gpkg()

  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_keep_areas_gt <- function(an_area = NULL, lower_bound = 0) {
  checkmate::check_class(an_area, "SpatialPolygons")
  checkmate::assert_numeric(lower_bound, len = 1, lower = 0.0)

  an_area <- an_area %>%
    .repair_area()

  an_area_agg <- an_area %>%
    raster::aggregate() %>%
    raster::disaggregate()

  if (an_area_agg %>% raster::crs() %>% is.na()){
    remain_areas_agg <- an_area_agg[rgeos::gArea(an_area_agg, byid = T) > lower_bound, ]
  }
  else {
    remain_areas_agg <- an_area_agg[raster::area(an_area_agg) > lower_bound, ]
  }

  if (remain_areas_agg@polygons %>% length() > 0){
    return(
      an_area %>%
        raster::intersect(remain_areas_agg)
    )
  } else {
    return(remain_areas_agg)
  }
}
