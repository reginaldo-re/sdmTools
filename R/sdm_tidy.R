#' Convert a \code{SDM_area} object to a dataframe.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#'
#' @param region An attribute used to join the primitive shapes to information about study area.
#' If it is equal to NULL, the information about study area is dropped from the resulting dataframe.
#'
#' @return The \code{SDM_area} object converted to a dataframe. The dataframe is  composed by attributes of the
#' primitive shapes (polygons or lines) in an object of package \code{sp}
#' (<https://cran.r-project.org/web/packages/sp>) and the information about study area.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- rgdal::readOGR(
#'    system.file("vect_files/brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = F
#' )
#'
#' gridded_area <- SPDF %>%
#'  sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
#'  make_grid(var_names = list(), new_name = T)
#'
#' a_df <- gridded_area %>%
#'    sdm_tidy(region = "cell_id")
#'
#' a_df %>%
#'    head(20)
#' }
sdm_tidy <- function(an_area, region = NULL){
  UseMethod("sdm_tidy", an_area)
}

#' @export
sdm_tidy.SDM_area <- function(an_area, region = NULL){
  an_area$study_area %>%
    .sdm_tidy(region) %>%
    return()
}

#' @export
sdm_tidy.SpatialPolygonsDataFrame <- function(an_area, region = NULL){
  an_area %>%
    .sdm_tidy(region) %>%
    return()
}

.sdm_tidy <- function(an_area, region = NULL) {
  suppressMessages(
    df_tmp <- an_area %>%
      broom::tidy()
  )
  if (!(region %>% is.null())){
    if ((region %in% (an_area %>% names()))){
      df_tmp <- df_tmp %>%
        mutate({{region}} := as.integer(id)) %>%
        select(-id) %>%
        left_join(an_area@data, by=region)
      return(df_tmp)
    }
  }

  return(df_tmp)
}
