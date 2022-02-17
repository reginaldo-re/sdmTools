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
merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_path = NULL){
  UseMethod("merge_scenario", an_area)
}

#' @export
merge_scenario.SDM_area <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_path = NULL) {
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }

  an_area %>%
    .sp_merge_scenario(to_merge_scenario, var_names, new_path) %>%
    return()
}


.sp_merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_path = NULL) {
  checkmate::assert_class(an_area, "SDM_area")
  checkmate::assert_class(to_merge_scenario, "SDM_scenario")
  checkmate::assert_list(var_names, types = c("character"), any.missing = F, all.missing = F, unique = T, min.len = 1)

  checkmate::assert(
    checkmate::check_string(new_path),
    checkmate::check_directory_exists(new_path)
  )

  if (new_path %>% fs::is_dir())
    new_path %>%
      fs::dir_delete()


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

  if (to_merge_scenario$is_rast){
    .sp_merge_scenario_rast(an_area, to_merge_scenario, var_names, to_merge_scenario$content, new_path)
    # grid_variaveis_futuro$study_area@data <- grid_variaveis_futuro$study_area@data %>%
    #   mutate_at(nome_variaveis_futuro_x10 %>% unlist(), list(~./10))
  } else {
    .sp_merge_scenario_vect(an_area, to_merge_scenario, var_names, to_merge_scenario$content, new_path)
  }

  parallel::stopCluster(cl)
}


.sp_merge_scenario_rast  <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, an_element = NULL, new_path = NULL){
    atomic_scenario_list <- an_element %>%
      discard(~ is.list(.))

    nested_scenario <- an_element %>%
      keep(~ is.list(.))

    if (atomic_scenario_list %>% length() > 0){
      if (var_names %>% is.null()){
        var_names <- atomic_scenario_list %>%
          unlist() %>%
          unname() %>%
          fs::path_file() %>%
          unique() %>%
          fs::path_ext_remove() %>%
          as.list()
      }

      #foreach::foreach(
      #  atomic_scenario = atomic_scenario_list %>% unlist() %>% unname() %>% fs::path_dir() %>% unique(),
      #  .packages=c("sdmTools", "magrittr", "stringr", "fs", "dplyr")) %dopar% {
      for (atomic_scenario in atomic_scenario_list %>% unlist() %>% unname() %>% fs::path_dir() %>% unique()){
        tmp_area <- an_area %>%
          merge_area(
            to_merge_area = atomic_scenario,
            new_name = atomic_scenario %>% stringr::str_remove(paste0(a_scenario$path, "/", a_scenario$name, "/")) ,
            var_names = var_names)

        tmp_area$study_area@data <- tmp_area$study_area@data  %>%
          dplyr::select(ATTR_CONTROL_NAMES %>% as_vector() %>% dplyr::any_of()) %>%
          dplyr::bind_cols(
            tmp_area$study_area@data  %>%
              dplyr::select(-(an_area$study_area@data %>% names()))
          )

        tmp_area %>%
          inset("name", atomic_scenario %>% fs::path_file()) %>%
          save_gpkg(
            file_path = new_path %>%
              fs::path(
                atomic_scenario %>%
                  stringr::str_remove(a_scenario$path %>% fs::path(a_scenario$name))
              )
          )
        rm(tmp_area)
        gc()
      }
    }

    nested_scenario %>%
      map(~ .sp_merge_scenario_rast(an_area, a_scenario, var_names, ., new_path))
  }


.sp_merge_scenario_vect <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, an_element = NULL, new_path = NULL){
  atomic_scenario_list <- an_element %>%
    discard(~ is.list(.))

  nested_scenario <- an_element %>%
    keep(~ is.list(.))

  if (atomic_scenario_list %>% length() > 0){
    for (atomic_scenario in atomic_scenario_list){
      vect_file <- atomic_scenario %>%
          rgdal::readOGR(verbose = F)

      if (var_names %>% is.null()){
        var_names <- vect_file@data %>% names()
      }

      var_not_found <- var_names %>%
        map_chr(~ ifelse (.x %>% magrittr::is_in(vect_file %>% names()) %>% magrittr::not(), .x, "")) %>%
        discard(. == "") %>%
        paste(collapse = ", ")

      if ((var_not_found %>% nchar() > 0) %>% all()){
        stop("Variables not found:" %>% paste(var_not_found))
      }

      vect_file@data <- vect_file@data %>%
        select(var_names %>% all_of())

      tmp_dir_name <- tempdir() %>%
        fs::path(
          atomic_scenario %>%
            stringr::str_remove(paste0(a_scenario$path, "/")) %>%
            stringr::str_remove(paste0(a_scenario$name, "/")) %>%
            fs::path_ext_remove()
        )

      tmp_dir_name %>%
        fs::dir_create()
      for (var_name in var_names){
        raster_grid <- gdalUtils::gdal_rasterize(
          atomic_scenario,
          tmp_dir_name %>% fs::path(paste0(var_name,".tif")),
          #burn = 0,
          a = var_name,
          #at = T,
          co = c("BIGTIFF=YES"),
          a_nodata = "-9999.0",
          tr = an_area$resolution / 10,
          #tap= T,
          ot = 'Float32',
          output_Raster = T,
          te = an_area$study_area %>% raster::bbox() %>% as("vector"),
          verbose = T
        )
      }
    }
  }

  nested_scenario %>%
    map(~ .sp_merge_scenario_vect(an_area, a_scenario, var_names, ., new_path))
}

