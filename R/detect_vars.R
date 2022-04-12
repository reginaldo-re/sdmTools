#' @noRd
#' @keywords internal
detect_vars <- function(an_area = NULL, var_names = NULL){
  assert(
    var_names %>%
      check_list(types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    var_names %>%
      check_character(any.missing = F, all.missing = T, unique = T, null.ok = T),
    msg = "The variable names argument (var_names) must be:" %>%
      paste("a vector/list of non duplicated strings to be selected.") %>%
      paste("an empty list/vector to select none variable; or,") %>%
      paste("NULL to select all available variables.")
  )
  UseMethod("detect_vars", an_area)
}

#' @noRd
#' @keywords internal
detect_vars.Spatial <- function(an_area = NULL, var_names = NULL){
  return(list())
}

#' @noRd
#' @keywords internal
detect_vars.SpatialPolygonsDataFrame <- function(an_area = NULL, var_names = NULL){
  return(
    an_area %>%
      names() %>%
      .detect_vars(var_names)
  )
}

#' @noRd
#' @keywords internal
detect_vars.SpatialLinesDataFrame <- function(an_area = NULL, var_names = NULL){
  return(
    an_area %>%
      names() %>%
      .detect_vars(var_names)
  )
}


#' @noRd
#' @keywords internal
detect_vars.SDM_area <- function(an_area = NULL, var_names = NULL){
  return(
    an_area$study_area %>%
      names() %>%
      .detect_vars(var_names)
  )
}

#' @noRd
#' @keywords internal
detect_vars.SDM_scenario <- function(an_area = NULL, var_names = NULL){
  an_area %>%
    check_scenario()

  if (an_area$is_rast){
    return(
      an_area$content %>%
        pluck(1) %>%
        unlist() %>%
        path_file() %>%
        path_ext_remove() %>%
        .detect_vars(var_names)
    )
  } else {
    return(
      an_area$content %>%
        pluck(1) %>%
        readOGR(verbose = F) %>%
        names() %>%
        .detect_vars(var_names)
    )
  }
}


#' @noRd
#' @keywords internal
detect_vars.character <- function(an_area = NULL, var_names = NULL){
  assert(
    check_directory_exists(an_area),
    check_file_exists(an_area, extension = VECT_FORMATS_EXT %>% as_vector()),
    check_character(an_area, any.missing = F, all.missing = F, min.len = 1, unique = T),
    msg = "The study area (an_area) must be:" %>%
      paste("an existing directory containing vector or raster files; or,") %>%
      paste("a vector file name or a raster file name; or,") %>%
      paste("a vector of non empty or duplicated strings represeting variable names.")
  )

  if (test_directory_exists(an_area)){
    file_list <- an_area %>%
      dir_ls(recurse = F, type = "file")

    file_list %>%
      assert_character(
        any.missing = F,
        all.missing = F,
        min.len = 1,
        unique = T,
        msg = "There must be a list o valid files whether a study area (an_area) is a directory."
      )

    file_types <- file_list %>%
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
        choices = RAST_FORMATS_EXT %>% as_vector(),
        empty.ok = F,
        msg = "The file type encountered in the scenario (a_scenario) must be a valid raster."
      )


    dir_list <- an_area %>%
      dir_ls(recurse = F, type = "directory")
    assert_character(dir_list, len = 0)

    return(
      file_list %>%
        path_file() %>%
        path_ext_remove() %>%
        .detect_vars(var_names)
    )
  }
  else if (test_file_exists(an_area, extension = VECT_FORMATS_EXT %>% as_vector())){
    an_area %>%
      length() %>%
      assert_int(
        lower = 1,
        upper = 1,
        msg = "Only one file is accepted if the study area (an_area) is a file name!"
      )

    return(
      an_area %>%
        readOGR(verbose = F) %>%
        names() %>%
        .detect_vars(var_names)
    )
  }
  else if (test_file_exists(an_area, extension = RAST_FORMATS_EXT %>% as_vector())){
    an_area %>%
      length() %>%
      assert_int(
        lower = 1,
        upper = 1,
        msg = "Only one file is accepted if the study area (an_area) is a file name!"
      )
    return(
      an_area %>%
        path_file() %>%
        path_ext_remove() %>%
        .detect_vars(var_names)
    )
  }
  else {
    return(
      an_area %>%
        .detect_vars(var_names)
      )
  }
}

#' @noRd
#' @keywords internal
.detect_vars <- function(an_area_names = NULL, var_names = NULL){
  assert(
    check_list(an_area_names, types = "character", any.missing = F, all.missing = F, unique = T),
    check_character(an_area_names, any.missing = F, all.missing = F, unique = T),
    msg = "The variable names (an_area_names) must be:" %>%
      paste("a vector/list of non duplicated strings represeting all available variable names.")
  )
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T),
    msg = "The variable names (var_names) must be:" %>%
      paste("a vector/list of non duplicated strings to be selected.") %>%
      paste("an empty list/vector to select none variable; or,") %>%
      paste("NULL to select all available variables.")
  )
  if(an_area_names %>% length() == 0){
    return(list())
  }
  if (var_names %>% is.null()){
    return(an_area_names)
  }
  if (var_names %>% length() == 0){
    return(list())
  }

  var_found <- var_names %>%
    map(~
      ifelse(an_area_names %>% str_detect(fixed(.x, ignore_case = T)), .x, "") %>%
      discard(. == "")
    ) %>%
    compact()

  has_ambiguous_var <- var_found %>%
    map(~ ifelse(length(.) != 1, T, F)) %>%
    unlist() %>%
    any()

  has_ambiguous_var %>%
    assert_false(
      msg = "At least one variable name is ambiguous. Try to use more specific variable names."
    )

  var_found <- var_found  %>%
    keep(~ length(.) == 1) %>%
    unlist()

  if (var_found %>% is.null()){
    var_found <- list()
  }

  return(var_found)
}
