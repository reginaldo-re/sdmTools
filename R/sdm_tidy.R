#' @export
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
