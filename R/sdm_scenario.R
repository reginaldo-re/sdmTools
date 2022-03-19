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
#'  dir_path("scenarios_folder")
#'
#' system.file("rast_files", package="sdmTools") %>%
#'  dir_copy(a_dir, overwrite = T)

#' tmp_scenario <- a_dir %>%
#'  sdm_scenario()
#' }
sdm_scenario <- function(a_scenario = NULL, var_names = NULL){
  assert_directory_exists(a_scenario)
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )

  UseMethod("sdm_scenario", a_scenario)
}

#' @export
sdm_scenario.character <- function(a_scenario = NULL, var_names = NULL){
  return(
    a_scenario %>%
      .sdm_scenario(var_names)
  )
}

#' @noRd
#' @keywords internal
.sdm_scenario <- function(a_scenario = NULL, var_names = NULL){
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )
  file_types <- a_scenario %>%
    dir_ls(recurse = T, type = "file") %>%
    path_ext() %>%
    unique()
  assert_int(length(file_types), lower = 1, upper = 1, .var.name = "File types.")
  assert_subset(file_types, c(as_vector(RAST_FORMATS_EXT), as_vector(VECT_FORMATS_EXT)), empty.ok = F)
  assert_true(check_scenario(a_scenario))

  tmp_content <- NULL
  if (RAST_FORMATS_EXT %>% contains(file_types)){
    tmp_content <- a_scenario %>%
      path_dir() %>%
      .find_scenario_rast_files(
        base_name = a_scenario %>% path_file() %>% path_ext_remove(),
        var_names = var_names
      )

    sdm_scenario_tmp <- list(
      sdm_scenario_name = a_scenario %>% path_file() %>%  path_ext_remove(),
      dir_path = a_scenario %>% path_dir(),
      is_rast = T,
      content = tmp_content
    )
  } else {
    tmp_content <- a_scenario %>%
      path_dir() %>%
      .find_scenario_vect_files(
        base_name = a_scenario %>% path_file() %>% path_ext_remove(),
        var_names = var_names
      )

    sdm_scenario_tmp <- list(
      sdm_scenario_name = a_scenario %>% path_file() %>%  path_ext_remove(),
      dir_path = a_scenario %>% path_dir(),
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
  file_list <- base_path %>%
    path(base_name) %>%
    dir_ls(type = "file")
  dir_list <- base_path %>%
    path(base_name) %>%
    dir_ls(type = "dir")

  if (file_list %>% length() > 0 && dir_list %>% length() > 0){
    "Invalid scenario folder. Scenario folder must be hierarchically a raster or a list of rasters folders." %>%
      abort()
  }

  if (file_list %>% length() > 0){
    var_found <- file_list %>%
      path_dir() %>%
      unique() %>%
      detect_vars(var_names) %>%
      compact() %>%
      unlist()

    if (var_found %>% is_empty()){
      "None variables found in to_merge_area." %>%
        abort()
    }

    var_not_found <- var_names %>%
      setdiff(var_found) %>%
      unlist(recursive = T)

    if (test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
      c(
        "Variables not found:",
        var_not_found
      ) %>%
        abort()
    }

    file_list <- file_list %>%
      keep(~ .x %>% str_detect(fixed(var_names %>% unlist(), ignore_case = T)) %>% any())

    return(file_list)
  } else {
    return(
      dir_list %>%
        set_names(base_name %>% rep(dir_list %>% length()) %>% path(dir_list %>% path_file())) %>%
        map(~ .find_scenario_rast_files(base_path, str_remove(., paste0(base_path, "/")), var_names))
    )
  }
}


#' @noRd
#' @keywords internal
.find_scenario_vect_files <- function(base_path = NULL, base_name = NULL, var_names = NULL){
  file_list <- base_path %>%
    path(base_name) %>%
    dir_ls(type = "file")
  dir_list <- base_path %>%
    path(base_name) %>%
    dir_ls(type = "dir")

  if (file_list %>% length() > 0){
    var_found <- file_list %>%
      map(~ .x %>% detect_vars(var_names)) %>%
      compact()

    if (var_found %>% is_empty()){
      "None variable found in to_merge_area." %>%
        abort()
    }

    var_not_found <- var_found %>%
      map(~ var_names %>% setdiff(.x)) %>%
      unlist(recursive = T)

    if (test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
      c(
        "Variables not found:",
        var_not_found %>%
          paste(names(.), ., sep = ":")
      ) %>%
        abort()
    }

    return(file_list)
  } else {
    return(
      dir_list %>%
        set_names(base_name %>% rep(dir_list %>% length()) %>% path(dir_list %>% path_file())) %>%
        map(~ .find_scenario_vect_files(base_path, str_remove(., paste0(base_path, "/")), var_names))
    )
  }
}

