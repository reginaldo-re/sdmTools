#' @export
save_gpkg <- function(an_area, file_name, file_path){
  UseMethod("save_gpkg", an_area)
}

#' @export
save_gpkg.SDM_area <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  if (file_name %>% is.null() || file_name == "" || file_name %>% fs::path_ext() == "") {
    file_name <- .guess_file_name(an_area)
  }
  .sp_save_gpkg(an_area$study_area, file_name, file_path)
}


#' @export
save_gpkg.Spatial <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  .sp_save_gpkg(an_area, file_name, file_path)
}

.sp_save_gpkg <- function(an_area = NULL, file_name = NULL, file_path = NULL){
  checkmate::check_class(an_area, "Spatial")
  checkmate::assert_string(file_name)

  if (file_name %>% fs::path_dir() != "."){
    checkmate::check_null(file_path)
    file_path <- file_name %>% fs::path_dir()
  }
  checkmate::assert_string(file_path)

  file_name <- file_name %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    paste0(".gpkg")

  result = tryCatch({
    clear_dir <- file_path %>%
      fs::dir_exists() %>%
      not()

    file_path %>%
      fs::dir_create(recurse = T)

    an_area %>% rgdal::writeOGR(
      dsn = file_path %>% fs::path(file_name),
      layer = file_name %>% fs::path_file() %>% fs::path_ext_remove(),
      driver="GPKG",
      overwrite_layer = T
    )
  }, error = function(e){
    e$message %>% print()
    "Error saving file:" %>%
      paste0(fs::path(file_path, file_name)) %>%
      paste0(".")

    if (clear_dir){
      file_path %>%
        fs::dir_delete()
    }
  })
}
