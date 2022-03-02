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
#'  areas_gt(
#'     0.25,
#'     new_name = "Removing a single area from a SpatialPolygons study area."
#'  )
#'
#' plot(new_area$study_area)
#' }
areas_gt <- function(an_area = NULL, lower_bound = 0, new_name = F){
  UseMethod("areas_gt")
}

#' @export
areas_gt.SDM_area <- function(an_area = NULL, lower_bound = 0, new_name = F) {
  checkmate::check_class(an_area$study_area, "SpatialPolygons")

  checkmate::assert(
    checkmate::check_string(new_name),
    checkmate::check_logical(new_name, len = 1)
  )

  an_area$study_area <- an_area$study_area %>%
    .sp_areas_gt(lower_bound)

  if (checkmate::test_logical(new_name, min.len = 1)){
    if (new_name) {
      an_area$name <- an_area$name %>%
        paste0("_drop")
    }
  } else if (checkmate::test_string(new_name)){
    an_area$name <- new_name
  }

  return(an_area)
}

#' @noRd
#' @keywords internal
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
    return(
      an_area %>%
        raster::intersect(remain_areas_agg)
    )
  } else {
    return(remain_areas_agg)
  }
}
