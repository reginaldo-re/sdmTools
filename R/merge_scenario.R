#' Title
#'
#' @param an_area
#' @param to_merge_scenario
#' @param output_path
#' @param var_names
#'
#' @return
#' @export
#'
#' @examples
merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, output_path = NULL){
  UseMethod("merge_scenario", an_area)
}

#' @export
merge_scenario.SDM_area <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, output_path = NULL) {
  checkmate::assert_list(an_area %>% detect_vars(var_names), max.len = 0)
  an_area %>% detect_vars(var_names) %>% length() > 0
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }

  an_area$study_area <- an_area %>%
      .sp_merge_scenario(
        to_merge_scenario = to_merge_scenario,
        var_names = var_names,
        output_path = output_path
      )
  return(an_area)
}


#' @noRd
#' @keywords internal
.sp_merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, output_path = NULL) {
  checkmate::assert_class(an_area, "SDM_area")
  checkmate::assert_class(to_merge_scenario, "SDM_scenario")
  checkmate::assert_list(var_names, types = c("character"), any.missing = F, all.missing = F, unique = T, min.len = 1)
  checkmate::assert_string(output_path)
  checkmate::assert_subset(var_names %>% unlist(), choices = detect_vars(to_merge_scenario, var_names))

  if (output_path %>% fs::is_dir())
    output_path %>%
      fs::dir_delete()
  output_path %>%
    fs::dir_create()

  if (to_merge_scenario$is_rast){
    .sp_merge_scenario_rast(
      an_area = an_area,
      a_scenario = to_merge_scenario,
      var_names = var_names,
      an_element = to_merge_scenario$content,
      output_path = output_path
    )
    # grid_variaveis_futuro$study_area@data <- grid_variaveis_futuro$study_area@data %>%
    #   mutate_at(nome_variaveis_futuro_x10 %>% unlist(), list(~./10))
  } else {
    tmp_dir <- tempdir() %>%
      fs::path("merging")

    if (tmp_dir %>% fs::dir_exists()){
      tmp_dir %>%
        fs::dir_delete()
    }
    tmp_dir %>%
      fs::dir_create()

    .sp_merge_scenario_vect(
      an_area = an_area,
      a_scenario = to_merge_scenario,
      var_names = var_names,
      an_element = to_merge_scenario$content,
      output_path = output_path
    )

    tmp_scenario <- tmp_dir %>% sdm_scenario(var_names)
    .sp_merge_scenario_rast(
      an_area = an_area,
      a_scenario = tmp_scenario,
      var_names = var_names,
      an_element = tmp_scenario$content,
      output_path = output_path
    )
  }

  if (fs::dir_ls(recurse = T, type = "file", glob = "*.gpkg") %>% length() > 0 ){
    col_names <- an_area$study_area@data %>% names()
    tmp_area <- output_path %>%
      fs::dir_ls(recurse = T, type = "file", glob = "*.gpkg") %>%
      pluck(1) %>%
      rgdal::readOGR(verbose = F)

    tmp_area@data <- tmp_area %>%
      slot("data") %>%
      select(c(ATTR_CONTROL_NAMES$x_centroid, ATTR_CONTROL_NAMES$y_centroid)) %>%
      inner_join(
        an_area$study_area@data,
        by = c(ATTR_CONTROL_NAMES$x_centroid, ATTR_CONTROL_NAMES$y_centroid)
      ) %>%
      select(col_names %>% all_of()) %>%
      mutate_at(ATTR_CONTROL_NAMES$cell_id, ~ 1:nrow(tmp_area@data))

    an_area <- tmp_area
  }
  return(an_area)
}


#' @noRd
#' @keywords internal
.sp_merge_scenario_rast  <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, an_element = NULL, output_path = NULL){
  atomic_scenario_list <- an_element %>%
    discard(~ is.list(.))

  nested_scenario <- an_element %>%
    keep(~ is.list(.))

  if (atomic_scenario_list %>% length() > 0){
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
    #foreach::foreach(
    #  atomic_scenario = atomic_scenario_list %>% unlist() %>% unname() %>% fs::path_dir() %>% unique(),
    #  .packages=c("sdmTools", "magrittr", "stringr", "fs", "dplyr")) %dopar% {
    for (atomic_scenario in atomic_scenario_list %>% unlist() %>% unname() %>% fs::path_dir() %>% unique()){
      tmp_area <- an_area %>%
        merge_area(
          to_merge_area = atomic_scenario,
          new_name = atomic_scenario %>% stringr::str_remove(paste0(a_scenario$path, "/", a_scenario$name, "/")) ,
          var_names = var_names
        )

      tmp_area$study_area@data <- tmp_area$study_area@data  %>%
        dplyr::select(
          c(
            ATTR_CONTROL_NAMES %>% as_vector(),
            var_names
          ) %>%
            unlist() %>%
            any_of()
        )

      tmp_area %>%
        inset("name", atomic_scenario %>% fs::path_file()) %>%
        save_gpkg(
          file_path = output_path %>%
            fs::path(
              atomic_scenario %>%
                stringr::str_remove(a_scenario$path %>% fs::path(a_scenario$name))
            )
        )
      rm(tmp_area)
      gc()
    }
    parallel::stopCluster(cl)
  }

  nested_scenario %>%
    map(~
        .sp_merge_scenario_rast(
          an_area = an_area,
          a_scenario = a_scenario,
          var_names = var_names,
          an_element = .x,
          output_path = output_path
        )
    )
  }


#' @noRd
#' @keywords internal
.sp_merge_scenario_vect <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, an_element = NULL, output_path = NULL){
  atomic_scenario_list <- an_element %>%
    discard(~ is.list(.))

  nested_scenario <- an_element %>%
    keep(~ is.list(.))

  if (atomic_scenario_list %>% length() > 0){
    tmp_dir <- tempdir() %>%
      fs::path("merging")

    for (atomic_scenario in atomic_scenario_list){
      vect_file <- atomic_scenario %>%
          rgdal::readOGR(verbose = F)

      var_found <- vect_file@data %>%
        names() %>%
        keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any())

      var_found <- var_found %>%
        purrr::set_names(var_names)

      tmp_dir_scenario <- tmp_dir %>%
        fs::path(
          atomic_scenario %>%
            stringr::str_remove(paste0(a_scenario$path, "/")) %>%
            stringr::str_remove(paste0(a_scenario$name, "/")) %>%
            fs::path_ext_remove()
        )

      tmp_dir_scenario %>%
        fs::dir_create()

      for (var_name in var_names){
        raster_scenario <- gdalUtils::gdal_rasterize(
          src_datasource = atomic_scenario,
          dst_filename = tmp_dir_scenario %>% fs::path(paste0(var_name,".tif")),
          #burn = 0,
          a = var_found[[var_name]],
          at = T,
          co = c("BIGTIFF=YES"),
          a_nodata = "-9999.0",
          tr = an_area$resolution / 10,
          #tap= T,
          ot = 'Float32',
          output_Raster = T,
          te = an_area$study_area %>% raster::bbox() %>% as("vector"),
          verbose = F
        )
      }

      tmp_area <- an_area

      tmp_area$study_area@data <- tmp_area$study_area@data %>%
        select(ATTR_CONTROL_NAMES$cell_id)

      tmp_area <- tmp_area %>%
        merge_area(
          to_merge_area = tmp_dir_scenario,
          var_names = var_names,
          new_name = atomic_scenario %>% fs::path_file() %>% fs::path_ext_remove()
        )

      save_tif(
        an_area = tmp_area,
        file_path = tmp_dir %>%
          fs::path(
            atomic_scenario %>%
              stringr::str_remove(a_scenario$path %>% paste0("/")) %>%
              stringr::str_remove(a_scenario$name %>% paste0("/")) %>%
              fs::path_dir()
          ),
        file_name = atomic_scenario %>% fs::path_file() %>% fs::path_ext_remove()
      )
    }
  }

  nested_scenario %>%
    map(~
        .sp_merge_scenario_vect(
          an_area = an_area,
          a_scenario = a_scenario,
          var_names = var_names,
          an_element = .x,
          output_path = output_path
        )
    )
}

