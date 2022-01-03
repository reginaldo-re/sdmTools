#' Merge rasters over a gridded study area.
#'
#' @param an_area A SDM_area object with cells covering the study area.
#' @param area_source A path to a folder or a Raster* object with variables to merge with.
#' @param var_names A list of variable names to keep on cells. Variables area computed using
#' the average of raster points that over each cell. It try to match each variable name
#' (ignoring case) in the study area.
#'
#' @return A SDM_area_gridded object containing variables merged with. If the CRS of the Raster* is
#' different from the CRS of the SDM_area object, it is reproject. The merging process
#' produces cells intersecting SDM_area object and Raster*.
#' @export
#'
#' @examples
#' \dontrun{
#' aaa
#' }
merge_area <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL){
  UseMethod("merge_area", an_area)
}

#' @export
merge_area.default <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  an_area %>%
    return()
}

#' @export
merge_area.SDM_area <- function(an_area = NULL, to_merge_area = NULL, var_names=NULL){
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }
  an_area %>%
    .sp_merge_area(to_merge_area, var_names) %>%
    return()
}


.sp_merge_area <-function(an_area = NULL, to_merge_area = NULL, var_names = NULL){
  checkmate::check_class(an_area, "SDM_area")
  checkmate::assert(
    checkmate::check_file_exists(to_merge_area),
    checkmate::check_directory_exists(to_merge_area),
    checkmate::check_class(to_merge_area, "RasterLayer"),
    checkmate::check_class(to_merge_area, "RasterStack"),
    checkmate::check_class(to_merge_area, "RasterBrick")
  )
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, types = c("character"), any.missing = F, all.missing = F, unique = T)
  )

  if (var_names %>% is.null()){
    if (to_merge_area %>% fs::is_file()){
      to_merge_area <- to_merge_area %>%
        fs::path_dir()
    }
    var_names <- to_merge_area %>%
      fs::dir_ls(type = "file") %>%
      fs::path_file() %>%
      fs::path_ext_remove()
  }
  if (var_names %>% is.list()){
    var_names <- var_names %>%
      unlist()
  }

  raster_list <- to_merge_area %>%
    fs::dir_ls(type = "file") %>%
    purrr::keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any())

  if (raster_list %>% length() != var_names %>% length()){
    stop("At least one variable name is ambiguous. Try to use more specific variable names.")
  }

  raster_stack <- raster_list %>%
    raster::stack()

  result_crs <- suppressWarnings(
    try(raster::crs(raster_stack))
  )
  if (result_crs %>% class() == "try-error" || result_crs %>% is.na()){
    raster::crs(raster_stack) <- raster::crs("EPSG:4326")
  }

  raster_tmp_file <- tempfile() %>%
    paste0(".tif")

  raster_stack %>%
    raster::writeRaster(
      raster_tmp_file,
      format = "GTiff",
      bylayer = F, #bylayer = T,
      #suffix = lista_rasters_bio %>% names(),
      options = c("dstnodata =-9999.0"),
      overwrite=T,
    )

  shp_countour_file <- tempfile() %>%
    paste0(".gpkg")

  an_area$study_area %>%
    raster::aggregate(dissolve=T) %>%
    #rgeos::gBuffer(width=-(min(c(cell_width, cell_height)))/10, capStyle = "SQUARE", joinStyle = "BEVEL") %>%
    #rgeos::gUnionCascaded() %>%
    as("SpatialPolygonsDataFrame") %>%
    save_gpkg(shp_countour_file)

  shp_area_file <- tempfile() %>%
    paste0(".gpkg")

  an_area$study_area %>%
    save_gpkg(shp_area_file)

  shp_grid_file <- tempfile() %>%
    paste0(".gpkg")

  an_area$study_area %>%
    as("SpatialPolygonsDataFrame") %>%
    save_gpkg(shp_grid_file)

  raster_file_reescaled_countour <- tempfile() %>%
    paste0(".tif")

  raster_reescaled_countour <- gdalUtils::gdalwarp(
    raster_tmp_file,
    raster_file_reescaled_countour,
    s_srs = raster::crs(raster_stack),
    t_srs = raster::crs(an_area$crs),
    cutline = shp_countour_file,
    crop_to_cutline = T,
    r = 'average',
    tr = an_area$resolution,
    tap = T,
    te = an_area$study_area %>% raster::bbox() %>% as("vector"),
    te_srs = raster::crs(an_area$crs),
    dstnodata = "-9999.0",
    ot = 'Float32',
    co = c("BIGTIFF=YES"), #"COMPRESS=DEFLATE", "PREDICTOR=2","ZLEVEL=9"),
    #wo = c("CUTLINE_ALL_TOUCHED=TRUE"),
    multi = T,
    output_Raster = T,
    overwrite = T,
    verbose = F
  ) %>%
    raster::crop(an_area$study_area)

  raster_reescaled_countour_masked <- raster_reescaled_countour %>%
    terra::rast()

  raster_reescaled_countour_masked <- raster_reescaled_countour_masked %>%
    terra::mask(an_area$study_area %>% terra::vect(), touches=F) %>%
    raster::stack()

  raster_grid <- gdalUtils::gdal_rasterize(
    shp_area_file,
    tempfile() %>% paste0(".tif"),
    #burn = 0,
    a = "cell_id",
    #at = T,
    co = c("BIGTIFF=YES"),
    a_nodata = "-9999.0",
    tr = an_area$resolution,
    #tap= T,
    ot = 'Float32',
    output_Raster = T,
    te = an_area$study_area %>% raster::bbox() %>% as("vector"),
    verbose = F
  )

  raster_grid <- raster_grid %>%
    terra::rast() %>%
    terra::mask(
      raster_reescaled_countour_masked %>%
        terra::rast() %>%
        magrittr::extract2(1)
    )

  grid_cells <- raster_grid %>%
    as.vector() %>%
    purrr::discard(is.na)

  an_area$study_area@data <- an_area$study_area@data %>%
    as.data.frame()

  shp_grid <- an_area$study_area[grid_cells %>% as.integer(),]

  shp_grid@data$cell_id <- 1:length(grid_cells)
  shp_grid %>%
    sp::spChFIDs(1:length(grid_cells) %>% as.character())

  suppressMessages(
    shp_grid@data <- shp_grid@data %>% bind_cols(
      raster_reescaled_countour_masked %>%
        raster::as.list() %>%
        purrr::map_dfc(~ .x %>% raster::values() %>% purrr::discard(is.na) %>% as.data.frame()) %>%
        dplyr::rename_all(~ (var_names %>% unlist()))
    )
  )
  an_area$study_area <-shp_grid

  an_area %>%
    return()
}
