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
  file_types <- a_scenario %>% fs::dir_ls(recurse = T, type = "file") %>% fs::path_ext() %>% unique()
  checkmate::assert_int(length(file_types), lower = 1, upper = 1, .var.name = "File types.")
  checkmate::assert_subset(file_types, c(as_vector(RAST_FORMATS_EXT), as_vector(VECT_FORMATS_EXT)), empty.ok = F)

  tmp_content <- NULL
  if (RAST_FORMATS_EXT %>% contains(file_types)){
    tmp_content <- .find_scenario_rast_files(
      a_scenario %>% fs::path_dir(),
      a_scenario %>% fs::path_file() %>% fs::path_ext_remove(),
      var_names)

    sdm_scenario_tmp <- list(
      name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
      path = a_scenario %>% fs::path_dir(),
      content = tmp_content
    )

    if (sdm_scenario_tmp$content %>% is.list()){
      number_of_files <- sdm_scenario_tmp$content %>%
        unlist() %>%
        unname() %>%
        fs::path_file() %>%
        unique() %>%
        length()

      checkmate::assert_true(
        sdm_scenario_tmp %>% flatten_scenario() %>% every(~ length(.) == number_of_files),
        .var.name = "All rasters must have the same layers!"
      )
    }
  } else {
    tmp_content <- .find_scenario_vect_files(
      a_scenario %>% fs::path_dir(),
      a_scenario %>% fs::path_file() %>% fs::path_ext_remove(),
      var_names)

    sdm_scenario_tmp <- list(
      name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
      path = a_scenario %>% fs::path_dir(),
      content = tmp_content
    )
  }

  structure(
    sdm_scenario_tmp,
    class= "SDM_scenario"
  ) %>%
    return()
}

.find_scenario_rast_files <- function(base_path = NULL, base_name = NULL, var_names = NULL){
  file_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "file")
  dir_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0 && dir_list %>% length() > 0){
    stop("Invalid scenario folder. Scenario folder must be hierarchically a raster or a list of rasters folders.")
  }

  if (file_list %>% length() > 0){
    checkmate::assert_true(
      all(
        file_list %>%
          fs::path_ext() %>%
          magrittr::is_in(RAST_FORMATS_EXT %>% as_vector())
        ),
      .var.name = "It's a Raster?"
      )
    if (! var_names %>% is.null()){
      file_list <- file_list %>%
        purrr::keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any())
    }

    file_list %>%
      magrittr::set_names(base_name %>% rep(file_list %>% length())) %>%
      return()
  } else {
    dir_list %>%
      magrittr::set_names(base_name %>% rep(dir_list %>% length()) %>% fs::path(dir_list %>% fs::path_file())) %>%
      purrr::map(~ .find_scenario_rast_files(base_path, stringr::str_remove(., paste0(base_path, "/")), var_names)) %>%
      return()
  }
}


.find_scenario_vect_files <- function(base_path = NULL, base_name = NULL, var_names = NULL){
  file_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "file")
  dir_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0){
    if (! var_names %>% is.null()){
      names_ok <- file_list %>%
        map(~ var_names %>% is_in(.x %>% rgdal::readOGR(verbose = F) %>% names()) %>% all()) %>%
        all()
      if(! names_ok){
        stop("Some vect file do not contain one or more variable!")
      }
    }

    file_list <- file_list %>%
      magrittr::set_names(base_name %>% rep(file_list %>% length()))
  }
  file_list %>%
    append(dir_list %>%
             magrittr::set_names(base_name %>% rep(dir_list %>% length()) %>% fs::path(dir_list %>% fs::path_file())) %>%
             purrr::map(~ .find_scenario_vect_files(base_path, stringr::str_remove(., paste0(base_path, "/")), var_names))) %>%
    return()
}


flatten_scenario <- function(a_scenario) {
  .flatten_scenario <- function(an_element) {
    an_element %>%
      purrr::discard(~ is.list(.)) %>%
      append(an_element %>%
               purrr::keep(~ is.list(.)) %>%
               map(~ .flatten_scenario(.)) %>%
               flatten()) %>%
      return()
  }
  a_scenario$content %>%
    .flatten_scenario() %>%
    return()
}


