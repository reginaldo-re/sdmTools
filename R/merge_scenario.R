
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

  cl <- (parallel::detectCores() - 1) %T>%
    parallel::makeCluster() %>%
    doParallel::registerDoParallel()


  # cenarios_futuros <- pasta_cenarios_futuros %>%
  #   dir_ls()
  #
  # result <- foreach(pasta = cenarios_futuros, .packages=c("magrittr", "sdmTools", "janitor", "fs", "dplyr")) %dopar% {
  #   grid_variaveis_futuro <- shape_grid_estudo %>%
  #     merge_area(pasta, nome_variaveis_futuro, new_name = T)
  #   grid_variaveis_futuro$name <- pasta %>%
  #     fs::path_file() %>%
  #     make_clean_names()
  #
  #   grid_variaveis_futuro$study_area@data <- grid_variaveis_futuro$study_area@data %>%
  #     mutate_at(nome_variaveis_futuro_x10 %>% unlist(), list(~./10))
  #
  #   grid_variaveis_futuro %>%
  #     save_gpkg(file_path = here::here("output_data"))
  #
  #   rm(grid_variaveis_futuro)
  #   gc()
  # }

  stopCluster(cl)
}
