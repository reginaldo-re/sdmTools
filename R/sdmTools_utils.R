utils::globalVariables(c(":="))

#' @export
sdm_tidy <- function(an_area, region = NULL){
  UseMethod("sdm_tidy", an_area)
}

#' @export
sdm_tidy.SpatialPolygonsDataFrame <- function(an_area, region = NULL){
  an_area %>%
    .sdm_tidy(region) %>%
    return()
}

#' @export
sdm_tidy.SDM_area <- function(an_area, region = NULL){
  an_area$study_area %>%
    .sdm_tidy(region) %>%
    return()
}

.sdm_tidy <- function(an_area, region = NULL) {
  suppressMessages(
    df_tmp <- an_area %>%
      broom::tidy()
  )
  if (!(region %>% is.null())){
    if ((region %in% (an_area %>% names()))){
      df_tmp <- df_tmp %>%
        mutate({{region}} := as.integer(id)) %>%
        select(-id) %>%
        left_join(an_area@data, by=region)
      return(df_tmp)
    }
  }

  return(df_tmp)
}

#' @export

save_gpkg <- function(an_area, file_path, file_name){
  UseMethod("save_gpkg", an_area)
}

#' @export
save_gpkg.Spatial <- function(an_area = NULL, file_path = NULL, file_name = NULL){
  .save_gpkg_sp(an_area, file_path, file_name)
}

#' @export
save_gpkg.SDM_area <- function(an_area = NULL, file_path = NULL, file_name = NULL){
  if (file_name %>% is.null()){
    file_name <- paste(
      an_area$name,
      an_area$resolution[1],
      ifelse(!(is.null(an_area$epsg_code)), an_area$epsg_code, ""),
      ifelse(an_area$gridded, "gridded", "")
    ) %>%
      snakecase::to_snake_case() %>%
      paste0(".gpkg")

    file_path <- paste0(file_path, file_name)
  }
  .save_gpkg_sp(an_area$study_area, file_path, file_name)
}

.save_gpkg_sp <- function(an_area = NULL, file_path = NULL, file_name = NULL){
  if (an_area %>% is.null()){
    stop("an_area must be a object of type Spatial* or SDM_area.")
  }

  if (file_name %>% is.null()){
    stop("Invalid file name.")
  }

  if (file_name %>% fs::path_dir() != "."){
    file_path <- file_name %>% fs::path_dir()
    file_name <- file_name %>% fs::path_file()
  }
  if (file_path %>% is.null()){
    file_path <- "."
  }

  if (file_name %>% fs::path_ext() == ""){
    file_name <- paste0(file_path, ".gpkg")
  }
  result = tryCatch({
    clear_dir <- file_path %>%
      fs::path_dir() %>%
      fs::dir_exists() %>%
      not()

    file_path %>%
      fs::path_dir() %>%
      fs::dir_create(recurse = T)

    an_area %>% rgdal::writeOGR(
      dsn = file_path %>% fs::path(file_name),
      layer = file_name %>% fs::path_file() %>% fs::path_ext_remove(),
      driver="GPKG",
      overwrite_layer = T
    )
  }, error = function(e){
    print(e$message)
    print(paste0("Error saving file:", fs::path(file_path, file_name, ".")))
    if (clear_dir){
      file_path %>%
        fs::path_dir() %>%
        fs::dir_delete()
    }
  })
}

