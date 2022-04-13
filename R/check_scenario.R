#' @noRd
#' @keywords internal
check_scenario <- function(a_scenario = NULL){
  UseMethod("check_scenario", a_scenario)
}


#' @noRd
#' @keywords internal
check_scenario.SDM_scenario <- function(a_scenario = NULL){
  return(
    a_scenario$dir_path %>%
      path(a_scenario$sdm_scenario_name) %>%
      check_scenario.character()
  )
}


#' @noRd
#' @keywords internal
check_scenario.character <- function(a_scenario = NULL){
  a_scenario %>%
    assert_directory_exists(
      msg = "A scenario (a_scenario) must be a valid directory where data is stored."
    )
  file_types <- a_scenario %>%
    dir_ls(recurse = T, type = "file") %>%
    path_ext() %>%
    unique()

  file_types %>%
    length() %>%
    assert_int(
      lower = 1,
      upper = 1,
      msg = "There must be only one valid file type, or raster or vect, in the scenario (a_scenario)."
    )

  file_types %>%
    assert_subset(
      choices = c(as_vector(RAST_FORMATS_EXT), as_vector(VECT_FORMATS_EXT)),
      empty.ok = F,
      msg = "The file type encountered in the scenario (a_scenario) must be a valid raster or vect format."
    )

  if (RAST_FORMATS_EXT %>% contains(file_types) %>% all()){
    file_list <- .check_scenario(a_scenario)
    layer_names <- file_list %>%
      flatten_scenario() %>%
      pluck(1)

    invalid_layers <- file_list %>%
      flatten_scenario() %>%
      map(~ .x %>% identical(layer_names)) %>%
      keep(~ . == F)

    invalid_layers %>%
      length() %>%
      assert_int(
        upper = 0,
        msg = c(
          "Some raster folders have different layer names!",
          invalid_layers %>% names() %>% str_remove_all(a_scenario)
        )
      )

    return(TRUE)
  } else {
    file_list <- .check_scenario(a_scenario) %>%
      flatten_scenario()

    file_list <- file_list %>%
      map2(names(file_list), ~ .y %>% path(.x)) %>%
      unlist() %>%
      unname()

    file_list <- file_list %>%
      map(~ .x %>% readOGR(verbose = F) %>% names()) %>%
      set_names(file_list)

    var_names <- file_list %>%
      pluck(1)

    invalid_vars <- file_list %>%
      map(~ .x %>% identical(var_names)) %>%
      keep(~ . == F)

    invalid_vars %>%
      length() %>%
      assert_int(
        upper = 0,
        msg = c(
          "Some vect files have different var names!",
          invalid_layers %>% names() %>% str_remove_all(a_scenario)
        )
      )
    return(TRUE)
  }
}

#' @noRd
#' @keywords internal
.check_scenario <- function(dir_path = NULL){
  file_list <- dir_path %>%
    dir_ls(type = "file")
  dir_list <- dir_path %>%
    dir_ls(type = "dir")
  assert(
    check_false(file_list %>% length() > 0 && dir_list %>% length() > 0),
    check_false(file_list %>% length() == 0 && dir_list %>% length() == 0),
    combine = "and",
    msg = "Invalid scenario folder. Scenario folder must contains hierarchically a file or a list of files."
  )
  # if ((file_list %>% length() > 0 && dir_list %>% length() > 0) || (file_list %>% length() == 0 && dir_list %>% length() == 0)){
  #   "Invalid scenario folder. Scenario folder must contains hierarchically a file or a list of files." %>%
  #     abort()
  # }

  if (dir_list %>% length() > 0){
    return(
      dir_list %>%
        map(~ .x %>% .check_scenario())
    )
  } else {
    return(
      file_list %>%
        path_file()
    )
  }
}
