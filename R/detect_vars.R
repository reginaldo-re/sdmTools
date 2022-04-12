#' @noRd
#' @keywords internal
detect_vars <- function(an_area = NULL, var_names = NULL){
  assert(
    msg = "The variable names argument (var_names) should be NULL to select all available variables or at least one of the following options:",
    check_list(
      var_names,
      types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T,
      msg = "a vector/list of non duplicated strings to be selected."
    ),
    check_character(
      var_names,
      any.missing = F, all.missing = T, unique = T, null.ok = T,
      msg = "an empty list/vector to select none variable."
    )
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
    msg = "The study area (an_area) should be at least one of the following options:",
    an_area %>%
      check_directory_exists(
        msg = "an existing directory containing vector or raster files."
      ),
    an_area %>%
      check_file_exists(
        extension = VECT_FORMATS_EXT %>% as_vector(),
        msg = "a vector file name or a raster file name."
      ),
    an_area %>%
      check_character(
        any.missing = F, all.missing = F, min.len = 1, unique = T,
        msg = "a vector of non empty or duplicated strings representing variable names."
      )
  )

  if (test_directory_exists(an_area)){
    file_list <- an_area %>%
      dir_ls(recurse = F, type = "file")

    file_list %>%
      assert_character(
        msg = "There must be a list o valid files whether a study area (an_area) is a directory.",
        any.missing = F, all.missing = F, min.len = 1, unique = T
      )

    file_types <- file_list %>%
      path_ext() %>%
      unique()

    file_types %>%
      length() %>%
      assert_int(
        msg = "There must be only one valid file type, or raster or vect, in the scenario (a_scenario).",
        lower = 1, upper = 1
      )

    file_types %>%
      assert_subset(
        msg = "The file type encountered in the scenario (a_scenario) must be a valid raster.",
        choices = RAST_FORMATS_EXT %>% as_vector(), empty.ok = F,
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
        msg = "Only one file is accepted if the study area (an_area) is a file name!",
        lower = 1, upper = 1
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
        msg = "Only one file is accepted if the study area (an_area) is a file name!",
        lower = 1, upper = 1
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
    msg = "The variable names (an_area_names) should be" %>%
      paste("a vector/list of non duplicated strings represeting all available variable names."),
    an_area_names %>%
      check_list(
        types = "character", any.missing = F, all.missing = F, unique = T,
      ),
    an_area_names %>%
      check_character(
        any.missing = F, all.missing = F, unique = T,
      )
  )
  assert(
    msg = "The variable names argument (var_names) should be NULL to select all available variables or at least one of the following options:",
    var_names %>%
      check_list(
        types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "a vector/list of non duplicated strings to be selected."
      ),
    var_names %>%
      check_character(
        any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "an empty list/vector to select none variable."
      )
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
