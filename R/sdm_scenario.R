#' Creates a hierarchy of scenarios to use with SDM_area
#'
#' @param a_scenario A folder containing a hierarchy of inner folders. Leafs
#' of the folders must contain rasters.
#'
#' @return A SDM_scenario object containing the whole hierarchy.
#' @export
#'
#' @examples
#' \dontrun{
#' a_dir <- tempdir() %>%
#'  fs::path("scenarios_folder")
#'
#' system.file("rast_files", package="sdmTools") %>%
#'  fs::dir_copy(a_dir, overwrite = T)

#' tmp_scenario <- a_dir %>%
#'  sdm_scenario()
#' }
sdm_scenario <- function(a_scenario = NULL){
  UseMethod("sdm_scenario", a_scenario)
}

#' @export
sdm_scenario.character <- function(a_scenario = NULL){
  checkmate::assert_directory_exists(a_scenario)
  .sdm_scenario(a_scenario) %>%
    return()
}

.sdm_scenario <- function(a_scenario = NULL){
  sdm_scenario_tmp <- list(
    name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
    path = a_scenario,
    content = .find_scenario_files(a_scenario)
  )

  structure(
    sdm_scenario_tmp,
    class= "SDM_scenario"
  ) %>%
    return()
}

.find_scenario_files <- function(a_scenario = NULL){
  file_list <- a_scenario %>% fs::dir_ls(type = "file")
  dir_list <- a_scenario %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0 && dir_list %>% length() > 0){
    stop("Invalid scenario folder. Scenario folder must be hierarchically a raster or a list of rasters folders.")
  }

  if (file_list %>% length() > 0){
    withr::with_message_sink(
      tempfile(),
      {
        tmp_raster <- try(file_list %>% raster::stack(), silent = T)
      }
    )

    checkmate::assert_class(tmp_raster, "RasterStack")
    file_list %>%
      as.list() %>%
      return()
  } else {
    dir_list %>%
      purrr::map(~ .find_scenario_files(.)) %>%
      return()
  }
}



