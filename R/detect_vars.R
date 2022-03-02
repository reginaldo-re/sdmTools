#' @noRd
#' @keywords internal
detect_vars <- function(an_area = NULL, var_names = NULL){
  UseMethod("detect_vars", an_area)
}

#' @noRd
#' @keywords internal
detect_vars.Spatial <- function(an_area = NULL, var_names = NULL){
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
  return(
    an_area$content %>%
      pluck(1) %>%
      fs::path_file() %>%
      fs::path_ext_remove() %>%
      .detect_vars(var_names)
  )
}


#' @noRd
#' @keywords internal
detect_vars.character <- function(an_area = NULL, var_names = NULL){
  checkmate::assert(
    checkmate::check_directory_exists(an_area),
    checkmate::check_file_exists(an_area, extension = VECT_FORMATS_EXT %>% as_vector()),
    checkmate::check_character(an_area, any.missing = F, all.missing = F, min.len = 1, unique = T)
  )

  if (checkmate::test_directory_exists(an_area)){
    file_list <- an_area %>%
      fs::dir_ls(recurse = F, type = "file")
    checkmate::assert_character(file_list, any.missing = F, all.missing = F, min.len = 1, unique = T)

    file_types <- file_list %>%
      fs::path_ext() %>%
      unique()
    checkmate::assert_int(length(file_types), lower = 1, upper = 1, .var.name = "File types.")
    checkmate::assert_subset(file_types, as_vector(RAST_FORMATS_EXT), empty.ok = F)

    dir_list <- an_area %>%
      fs::dir_ls(recurse = F, type = "directory")
    checkmate::assert_character(dir_list, len = 0)

    return(
      file_list %>%
        fs::path_file() %>%
        fs::path_ext_remove() %>%
        .detect_vars(var_names)
    )
  }
  else if (checkmate::test_file_exists(an_area, extension = VECT_FORMATS_EXT %>% as_vector())){
    return(
      an_area %>%
        rgdal::readOGR(verbose = F) %>%
        names() %>%
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
  checkmate::assert(
    checkmate::check_list(an_area_names, types = "character", any.missing = F, all.missing = F, min.len = 1, unique = T),
    checkmate::check_character(an_area_names, any.missing = F, all.missing = F, min.len = 1, unique = T)
  )
  checkmate::assert(
    checkmate::check_list(var_names, types = "character", any.missing = F, all.missing = F, min.len = 1, unique = T),
    checkmate::check_character(var_names, any.missing = F, all.missing = F, min.len = 1, unique = T)
  )

  var_found <- var_names %>%
    map(~
      ifelse(an_area_names %>% stringr::str_detect(stringr::fixed(.x, ignore_case = T)), .x, "") %>%
      discard(. == "")
    ) %>%
    compact()

  has_ambiguous_var <- var_found %>%
    map(~ ifelse(length(.) != 1, T, F)) %>%
    unlist() %>%
    any()

  if (has_ambiguous_var){
    "At least one variable name is ambiguous. Try to use more specific variable names." %>%
      rlang::abort()
  }

  var_found <- var_found  %>%
    keep(~ length(.) == 1) %>%
    unlist()

  if (var_found %>% is.null()){
    var_found <- list()
  }

  return(var_found)
}
