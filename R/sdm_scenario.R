#' Creates a hierarchy of scenarios to use with SDM_area
#'
#' @param a_scenario A folder containing a hierarchy of inner folders. Leafs
#' of the folders must contain rasters.
#' @param var_names  A list of layer names to keep on rasters. It try to match each variable name
#' (ignoring case and partially matched)
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
sdm_scenario <- function(a_scenario = NULL, var_names = NULL){
  UseMethod("sdm_scenario", a_scenario)
}

#' @export
sdm_scenario.character <- function(a_scenario = NULL, var_names = NULL){
  checkmate::assert_directory_exists(a_scenario)
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, types = c("character"), any.missing = F, all.missing = F, unique = T)
  )

  if (var_names %>% is.list()){
    var_names <- var_names %>%
      unlist() %>%
      purrr::discard(~ . == "") %>%
      as.vector()
    checkmate::assert_vector(var_names, min.len = 1)
  }

  .sdm_scenario(a_scenario, var_names) %>%
    return()
}

.sdm_scenario <- function(a_scenario = NULL, var_names = NULL){
  sdm_scenario_tmp <- list(
    name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
    path = a_scenario,
    content = .find_scenario_files(a_scenario, var_names)
  )

  structure(
    sdm_scenario_tmp,
    class= "SDM_scenario"
  ) %>%
    return()
}

.find_scenario_files <- function(a_scenario = NULL, var_names = NULL){
  file_list <- a_scenario %>% fs::dir_ls(type = "file")
  dir_list <- a_scenario %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0 && dir_list %>% length() > 0){
    stop("Invalid scenario folder. Scenario folder must be hierarchically a raster or a list of rasters folders.")
  }

  if (file_list %>% length() > 0){
    checkmate::assert_true(
      all(
        file_list %>%
          fs::path_ext() %>%
          magrittr::is_in(RASTER_FORMATS_EXT %>% enum_as_vector())
        ),
      .var.name = "Is Raster?"
      )
    if (var_names %>% is.null()){
      file_list %>%
        as.list() %>%
        return()
    }
    else {
      file_list %>%
        purrr::keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any()) %>%
        return()
    }
  } else {
    dir_list %>%
      purrr::map(~ .find_scenario_files(., var_names)) %>%
      return()
  }
}



