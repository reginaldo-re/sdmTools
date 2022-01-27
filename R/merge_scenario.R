#' Title
#'
#' @param an_area
#' @param to_merge_scenario
#' @param new_path
#' @param var_names
#'
#' @return
#' @export
#'
#' @examples
merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, new_path = NULL, var_names = NULL){
  UseMethod("merge_scenario", an_area)
}


#' @export
merge_scenario.default <- function(an_area = NULL, to_merge_scenario = NULL, new_path = NULL, var_names = NULL) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  an_area %>%
    return()
}


#' @export
merge_scenario.SDM_area <- function(an_area = NULL, to_merge_scenario = NULL, new_path = NULL, var_names = NULL) {
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }

  an_area %>%
    .sp_merge_scenario(to_merge_scenario, new_path, var_names) %>%
    return()
}


.sp_merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, new_path = NULL, var_names = NULL) {
  checkmate::assert_class(an_area, "SDM_area")
  checkmate::assert_class(to_merge_scenario, "SDM_scenario")
  checkmate::assert(
    checkmate::check_string(new_path),
    checkmate::check_directory_exists(new_path)
  )
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, types = c("character"), unique = T)
  )

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores()
  }

  cl <- (num_workers - 1) %>%
    parallel::makeCluster() %T>%
    doParallel::registerDoParallel()

  raster_list <- to_merge_scenario %>%
    .flatten_scenario()


  result <- foreach::foreach(a_raster = raster_list %>% names() %>% unique(), .packages=c("magrittr", "janitor", "fs", "dplyr")) %dopar% {
    aaa <- raster_list %>% purrr::keep(names(.) == a_raster)
    print(aaa)

    # grid_variaveis_futuro <- shape_grid_estudo %>%
    #   merge_area(pasta, nome_variaveis_futuro, new_name = T)
    # grid_variaveis_futuro$name <- pasta %>%
    #   fs::path_file() %>%
    #   make_clean_names()
    #
    # grid_variaveis_futuro$study_area@data <- grid_variaveis_futuro$study_area@data %>%
    #   mutate_at(nome_variaveis_futuro_x10 %>% unlist(), list(~./10))
    #
    # grid_variaveis_futuro %>%
    #   save_gpkg(file_path = here::here("output_data"))
    #
    # rm(grid_variaveis_futuro)
    # gc()
  }

  parallel::stopCluster(cl)
}

.flatten_scenario <- function(a_scenario) {
  get_raster_list <- function(an_element) {
    atomic_elements <- an_element %>%
      purrr::keep(~ !is.list(.))

    nested_elements <- an_element %>%
      purrr::keep(~ is.list(.))

    raster_list <- NULL
    if (nested_elements %>% length() > 0){
      raster_list <- nested_elements %>%
        purrr::flatten() %>%
        get_raster_list()

      raster_list <- nested_elements %>%
        names() %>%
        fs::path(raster_list)
    }
    raster_list <- atomic_elements %>%
      purrr::map2(atomic_elements %>% names(), ~ .y %>% fs::path(.x)) %>%
      purrr::flatten_chr() %>%
      append(raster_list)

    raster_list %>%
      magrittr::set_names(raster_list %>% purrr::map(~ fs::path_dir(.)) %>% stringr::str_replace_all("/", ".")) %>%
      return()
  }
  raster_list <- a_scenario$content %>% get_raster_list()
  raster_list <- a_scenario$path %>%
    fs::path(raster_list) %>%
    as.list() %>%
    magrittr::set_names(raster_list %>% names()) %>%
    return()
}
