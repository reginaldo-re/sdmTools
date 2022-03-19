#' @keywords internal
repair_area <- function(an_area = NULL){
  UseMethod("repair_area", an_area)
}

repair_area.SpatialPolygons <- function(an_area = NULL){
  res_crs <- quiet(an_area %>% crs())

  if (res_crs %>% is("try-error") || res_crs %>% is.na()){
    "Invalid CRS." %>%
      abort()
  }

  quiet(
    an_area <- an_area %>% gBuffer(byid = TRUE, width = 0)
  )

  return(an_area)
}
