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

.sp_areas_gt <- function(an_area = NULL, lower_bound = 0) {
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
    an_area %>%
      raster::intersect(remain_areas_agg) %>%
      return()
  } else {
    remain_areas_agg %>%
      return()
  }
}
