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
  var_names <- var_names %>%
    unlist() %>%
    purrr::discard(~ . == "")

  checkmate::assert_character(var_names, any.missing = F, all.missing = F, unique = T, min.len = 1)

  return(
    a_scenario %>%
      .sdm_scenario(var_names)
  )
}

#' @noRd
#' @keywords internal
.sdm_scenario <- function(a_scenario = NULL, var_names = NULL){
  file_types <- a_scenario %>%
    fs::dir_ls(recurse = T, type = "file") %>%
    fs::path_ext() %>%
    unique()
  checkmate::assert_int(length(file_types), lower = 1, upper = 1, .var.name = "File types.")
  checkmate::assert_subset(file_types, c(as_vector(RAST_FORMATS_EXT), as_vector(VECT_FORMATS_EXT)), empty.ok = F)
  checkmate::assert_true(check_scenario(a_scenario))

  tmp_content <- NULL
  if (RAST_FORMATS_EXT %>% contains(file_types)){
    tmp_content <- a_scenario %>%
      fs::path_dir() %>%
      .find_scenario_rast_files(
        a_scenario %>% fs::path_file() %>% fs::path_ext_remove(),
        var_names
      )

    sdm_scenario_tmp <- list(
      name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
      path = a_scenario %>% fs::path_dir(),
      is_rast = T,
      content = tmp_content
    )
  } else {
    tmp_content <- .find_scenario_vect_files(
      a_scenario %>% fs::path_dir(),
      a_scenario %>% fs::path_file() %>% fs::path_ext_remove(),
      var_names)

    sdm_scenario_tmp <- list(
      name = a_scenario %>% fs::path_file() %>%  fs::path_ext_remove(),
      path = a_scenario %>% fs::path_dir(),
      is_rast = F,
      content = tmp_content
    )
  }

  return(
    structure(
      sdm_scenario_tmp,
      class= "SDM_scenario"
    )
  )
}

#' @noRd
#' @keywords internal
.find_scenario_rast_files <- function(base_path = NULL, base_name = NULL, var_names = NULL){
  file_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "file")
  dir_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0 && dir_list %>% length() > 0){
    "Invalid scenario folder. Scenario folder must be hierarchically a raster or a list of rasters folders." %>%
      rlang::abort()
  }

  if (file_list %>% length() > 0){
    var_found <- file_list %>%
      fs::path_dir() %>%
      unique() %>%
      detect_vars(var_names) %>%
      purrr::compact() %>%
      unlist()

    #var_not_found <- var_names %>%
    #  extract(!var_names %>% magrittr::is_in(var_found))
    var_not_found <- var_names %>% setdiff(var_found)

    if (checkmate::test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
      c(
        "Variables not found:",
        var_not_found
      ) %>%
        rlang::abort()
    }

    file_list <- file_list %>%
      purrr::keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any())

    return(file_list)
  } else {
    return(
      dir_list %>%
        magrittr::set_names(base_name %>% rep(dir_list %>% length()) %>% fs::path(dir_list %>% fs::path_file())) %>%
        purrr::map(~ .find_scenario_rast_files(base_path, stringr::str_remove(., paste0(base_path, "/")), var_names))
    )
  }
}


#' @noRd
#' @keywords internal
.find_scenario_vect_files <- function(base_path = NULL, base_name = NULL, var_names = NULL){
  file_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "file")
  dir_list <- base_path %>% fs::path(base_name) %>% fs::dir_ls(type = "dir")

  if (file_list %>% length() > 0){
    var_found <- file_list %>%
      purrr::map(~ .x %>% detect_vars(var_names)) %>%
      purrr::compact()

    var_not_found <- var_found %>%
      purrr::map(~ var_names %>% setdiff(.x)) %>%
      unlist()

    if (checkmate::test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
      c(
        "Variables not found:",
        var_not_found %>%
          paste(names(.), ., sep = ":")
      ) %>%
        rlang::abort()
    }

    return(file_list)
  } else {
    return(
      dir_list %>%
        magrittr::set_names(base_name %>% rep(dir_list %>% length()) %>% fs::path(dir_list %>% fs::path_file())) %>%
        purrr::map(~ .find_scenario_vect_files(base_path, stringr::str_remove(., paste0(base_path, "/")), var_names))
    )
  }
}

