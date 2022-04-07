#' Title
#'
#' @param an_area
#' @param to_merge_scenario
#' @param dir_path
#' @param var_names
#'
#' @return
#' @export
#'
#' @examples
merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_name = NULL, dir_path = NULL){
  assert(
    check_class(to_merge_scenario, "SDM_scenario", ordered = T, null.ok = F),
    check_directory_exists(to_merge_scenario)
  )
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = F, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = F, unique = T, null.ok = T)
  )
  assert_string(new_name, min.chars = 1, null.ok = T)
  assert_string(dir_path, min.chars = 1, null.ok = T)

  if (!an_area$scenario %>% is.null()){
    "It is not possible to merge an area in a sdm_area when scenarios is not null! Please assign null to scenario." %>%
      abort()
  }

  UseMethod("merge_scenario", an_area)
}

#' @export
merge_scenario.SDM_area <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_name = NULL, dir_path = NULL) {
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area()
  }

  if (to_merge_scenario %>% class() %>% length() > 1){
    if (to_merge_scenario %>% class() %>% pluck(1) == "fs_path"){
      to_merge_scenario <- to_merge_scenario %>%
        as.character()
    }
  }

  if (to_merge_scenario %>% class() != "SDM_scenario"){
    to_merge_scenario <- to_merge_scenario %>%
      sdm_scenario(
        var_names = var_names
      )
  }

  an_area <- an_area %>%
    .sp_merge_scenario(
      to_merge_scenario = to_merge_scenario,
      var_names = var_names,
      new_name = new_name,
      dir_path = dir_path
    )

  an_area <- an_area %>%
    save_gpkg(
      new_name = new_name,
      dir_path = dir_path
    )

  return(an_area)
}


#' @noRd
#' @keywords internal
.sp_merge_scenario <- function(an_area = NULL, to_merge_scenario = NULL, var_names = NULL, new_name = NULL, dir_path = NULL) {
  an_area %>%
    assert_class(
      classes = "SDM_area",
      msg = "A modeling area (an_area) must be an object of SDM_area class."
    )

  to_merge_scenario %>%
    assert_class(
      classes = "SDM_scenario",
      msg = "A scenario (a_scenario) must be an object of SDM_scenario class."
    )

  var_names %>%
    unlist() %>%
    assert_subset(
        choices = detect_vars(to_merge_scenario, var_names),
        empty.ok = F,
        msg = "Variable names in the scenario (to_merge_scenario) must be in the variable names list (var_names) of the modeling area (an_area)."
      )
  assert_string(dir_path, min.chars = 1, null.ok = T)

  if (to_merge_scenario$is_rast){
    an_area %>%
      .sp_merge_scenario_rast(
        a_scenario = to_merge_scenario,
        var_names = var_names,
        an_element = to_merge_scenario$content,
        dir_path = dir_path
      )
    # grid_variaveis_futuro$study_area@data <- grid_variaveis_futuro$study_area@data %>%
    #   mutate_at(nome_variaveis_futuro_x10 %>% unlist(), list(~./10))
  } else {
    tmp_dir <- tempdir() %>%
      path("merging")

    if (tmp_dir %>% dir_exists()){
      tmp_dir %>%
        dir_delete()
    }
    tmp_dir %>%
      dir_create()

    an_area$study_area %>%
      .sp_merge_scenario_vect(
        a_scenario = to_merge_scenario,
        var_names = var_names,
        an_element = to_merge_scenario$content,
        dir_path = tmp_dir,
        resolution = an_area$resolution
      )

    tmp_scenario <- tmp_dir %>%
      sdm_scenario(var_names)

    an_area %>%
      .sp_merge_scenario_rast(
        a_scenario = tmp_scenario,
        var_names = var_names,
        an_element = tmp_scenario$content,
        dir_path = dir_path
      )
  }

  if (dir_path %>% dir_ls(recurse = T, type = "file", glob = "*.gpkg") %>% length() > 0){
    tmp_area <- dir_path %>%
      dir_ls(recurse = T, type = "file", glob = "*.gpkg") %>%
      pluck(1) %>%
      readOGR(verbose = F)

    col_names <- an_area$study_area@data %>%
      names() %>%
      discard(. %in% c(ATTR_CONTROL_NAMES %>% as_vector()))

    tmp_area@data <- tmp_area %>%
      slot("data") %>%
      select(c(ATTR_CONTROL_NAMES$x_centroid, ATTR_CONTROL_NAMES$y_centroid) %>% any_of()) %>%
      inner_join(
        an_area$study_area@data,
        by = c(ATTR_CONTROL_NAMES$x_centroid, ATTR_CONTROL_NAMES$y_centroid)
      ) %>%
      mutate_at(ATTR_CONTROL_NAMES$cell_id, ~ 1:nrow(tmp_area@data)) %>%
      select(c(ATTR_CONTROL_NAMES$cell_id, ATTR_CONTROL_NAMES$x_centroid, ATTR_CONTROL_NAMES$y_centroid, col_names) %>% any_of())

    an_area$study_area <- tmp_area
  }


  an_area$scenarios <- dir_path %>%
    path("scenarios") %>%
    sdm_scenario()

  an_area <- an_area %>%
    save_gpkg(
      new_name = new_name,
      dir_path = dir_path
    )

  return(an_area)
}


#' @noRd
#' @keywords internal
.sp_merge_scenario_rast  <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, dir_path = NULL, an_element = NULL){
  atomic_scenario_list <- an_element %>%
    discard(~ is.list(.))

  nested_scenario <- an_element %>%
    keep(~ is.list(.))

  if (atomic_scenario_list %>% length() > 0){
    #chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    #if (nzchar(chk) && chk == "TRUE") {
    #  # use 2 cores in CRAN/Travis/AppVeyor
    #  num_workers <- 2L
    #} else {
    #  # use all cores in devtools::test()
    #  num_workers <- parallel::detectCores()
    #}

    #cl <- (num_workers - 1) %>%
    #  parallel::makeCluster() %T>%
    #  doParallel::registerDoParallel()
    #foreach::foreach(
    #  atomic_scenario = atomic_scenario_list %>% unlist() %>% unname() %>% path_dir() %>% unique(),
    #  .packages=c("sdmTools", "magrittr", "stringr", "fs", "dplyr")) %dopar% {
    for (atomic_scenario in atomic_scenario_list %>% unlist() %>% unname() %>% path_dir() %>% unique()){
      tmp_area <- an_area$study_area %>%
        .sp_merge_area(
          to_merge_area = atomic_scenario,
          var_names = var_names,
          resolution = an_area$resolution
        )

      tmp_area@data <- tmp_area@data  %>%
        select(
          c(
            ATTR_CONTROL_NAMES %>% as_vector(),
            var_names
          ) %>%
            unlist() %>%
            any_of()
        )

      new_name <- atomic_scenario %>%
        str_remove(paste0(a_scenario$dir_path, "/")) %>%
        str_remove(paste0(a_scenario$sdm_scenario_name, "/")) %>%
        path_file()

      new_dir_path <- dir_path %>%
        path("scenarios") %>%
        path(
          atomic_scenario %>%
            str_remove(paste0(a_scenario$dir_path, "/")) %>%
            str_remove(paste0(a_scenario$sdm_scenario_name, "/")) %>%
            path_dir()
        )

      tmp_area %>%
        save_gpkg(
          new_name = new_name,
          dir_path = new_dir_path
        )
      rm(tmp_area)
      gc()
    }
    #parallel::stopCluster(cl)
  }

  nested_scenario %>%
    map(~ an_area %>%
        .sp_merge_scenario_rast(
          a_scenario = a_scenario,
          var_names = var_names,
          an_element = .x,
          dir_path = dir_path
        )
    )
}


#' @noRd
#' @keywords internal
.sp_merge_scenario_vect <- function(an_area = NULL, a_scenario = NULL, var_names = NULL, dir_path = NULL, an_element = NULL, resolution = NULL){
  atomic_scenario_list <- an_element %>%
    discard(~ is.list(.))

  nested_scenario <- an_element %>%
    keep(~ is.list(.))

  if (atomic_scenario_list %>% length() > 0){
    for (atomic_scenario in atomic_scenario_list){
      vect_file <- atomic_scenario %>%
        readOGR(verbose = F)

      var_found <- atomic_scenario %>%
        detect_vars(var_names)

      var_found <- vect_file@data %>%
        select(var_found %>% matches()) %>%
        names() %>%
        set_names(var_names)

      tmp_dir_scenario <- dir_path %>%
        path(
          atomic_scenario %>%
            str_remove(paste0(a_scenario$dir_path, "/")) %>%
            str_remove(paste0(a_scenario$sdm_scenario_name, "/")) %>%
            path_ext_remove()
        )

      tmp_dir_scenario %>%
        dir_create()

      for (var_name in var_names){
        raster_scenario <- gdal_rasterize(
          src_datasource = atomic_scenario,
          dst_filename = tmp_dir_scenario %>% path(paste0(var_name,".tif")),
          #burn = 0,
          a = var_found[[var_name]],
          at = T,
          co = c("BIGTIFF=YES"),
          a_nodata = "-9999.0",
          tr = c(resolution / 10, resolution / 10),
          #tap= T,
          ot = 'Float32',
          output_Raster = T,
          te = an_area %>% bbox() %>% as("vector"),
          verbose = F
        )
      }
    }
  }

  nested_scenario %>%
    map(~ an_area %>%
        .sp_merge_scenario_vect(
          a_scenario = a_scenario,
          var_names = var_names,
          an_element = .x,
          dir_path = dir_path,
          resolution = resolution
        )
    )
}

