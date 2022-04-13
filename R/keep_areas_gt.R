#' Drop non-contiguous polygons with area smaller or equal lower_bound.
#'
#' @param an_area A \code{SDM_area} object representing the area of study.
#' @param sdm_area_name A name to new area study after dropping smaller polygons.
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
#'     sdm_area_name = "Removing a single area from a SpatialPolygons study area."
#'  )
#'
#' plot(new_area$study_area)
#' }
keep_areas_gt <- function(an_area = NULL, lower_bound = 0, sdm_area_name = NULL, dir_path = NULL){
  an_area$study_area %>%
    assert_class(
      classes = "SpatialPolygons",
      msg = "A study area (an_area$study_area) must be an object of SpatialPolygons* class."
    )
  sdm_area_name %>%
    assert_string(min.chars = 1, null.ok = T)
  dir_path %>%
    assert_string(min.chars = 1, null.ok = T)

  if (!sdm_area_name %>% is.null()){
    an_area$sdm_area_name <- sdm_area_name %>%
      to_snake_case()
  }
  if (!dir_path %>% is.null()){
    an_area$dir_path <- dir_path
  }
  sdm_area_name <- an_area$sdm_area_name
  dir_path <- an_area$dir_path

  if(dir_path %>% dir_exists()){
    dir_path %>%
      dir_delete()
  }
  quiet(
    dir_path %>%
      dir_create()
  )
  dir_path %>%
    assert_directory_exists(
      msg = "A problem occurs on the directory creation (dir_path). A study area (an_area) must have a valid directory (dir_path) where data will be saved."
    )


  an_area$study_area <- an_area$study_area %>%
    .sp_keep_areas_gt(lower_bound)

  an_area %>%
    save_gpkg()

  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_keep_areas_gt <- function(an_area = NULL, lower_bound = 0) {
  an_area %>%
    assert_class(
      classes = "SpatialPolygons",
      msg = "A study area (an_area) must be an object of SpatialPolygons* class."
    )

  lower_bound %>%
    assert_number(
      lower = 0.0,
      msg = "The lower bound (lower_bound) must be informed."
    )

  an_area <- an_area %>%
    .repair_area()

  an_area_agg <- an_area %>%
    aggregate() %>%
    disaggregate()

  if (an_area_agg %>% crs() %>% is.na()){
    remain_areas_agg <- an_area_agg[gArea(an_area_agg, byid = T) > lower_bound, ]
  }
  else {
    remain_areas_agg <- an_area_agg[area(an_area_agg) > lower_bound, ]
  }

  if (remain_areas_agg@polygons %>% length() > 0){
    return(
      an_area %>%
        rgeos::intersect(remain_areas_agg)
    )
  } else {
    return(remain_areas_agg)
  }
}
