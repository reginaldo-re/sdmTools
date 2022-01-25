#' Merge rasters over a gridded study area.
#'
#' @param an_area A \code{SDM_area} object with cells covering the study area.
#' @param var_names A list of variable names to keep on cells. It try to match each variable name
#' (ignoring case and partially matched) in the study area. Variables are calculated using
#' the average of features (polygons or lines) coverage by each cell.
#' @param to_merge_area A path to a \code{Raster*} (\url{https://cran.r-project.org/web/packages/raster/})
#' object (folder or file) with variables to merge with.
#' @param new_name A name to new area study after merge rasters over area.
#' the average of features (polygons or lines) coverage by each cell.
#' @return A \code{SDM_area} object containing variables merged with. If the CRS of the \code{Raster*} is
#' different from the CRS of the \code{SDM_area} object, it is reproject. The merging process
#' produces cells intersecting \code{SDM_area} object and \code{Raster*}.
#' @export
#'
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("vect_files/brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = F
#' )
#'
#' gridded_area <- SPDF %>%
#'  sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
#'  make_grid(var_names = list(), new_name = T)
#'
#' gridded_area <- gridded_area %>%
#'    merge_area(
#'       system.file("rast_files", package="sdmTools"),
#'       var_names = list("bio_5m_01", "bio_5m_02")
#'    )
#'
#' gridded_area$study_area@data %>% head()
#' }
#'
merge_area <- function(an_area = NULL, to_merge_area = NULL, var_names = NULL, new_name = F){
  UseMethod("merge_area", an_area)
}

#' @export
merge_area.default <- function(an_area = NULL, to_merge_area = NULL, var_names = NULL, new_name = F) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  an_area %>%
    return()
}

#' @export
merge_area.SDM_area <- function(an_area = NULL, to_merge_area = NULL, var_names = NULL, new_name = F){
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(var_names)
  }

  an_area %>%
    .sp_merge_area(to_merge_area, var_names, new_name) %>%
    return()
}

.sp_merge_area <-function(an_area = NULL, to_merge_area = NULL, var_names = NULL, new_name = NULL){
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
    checkmate::check_list(var_names, types = c("character"), unique = T)
  )
  checkmate::assert(
    checkmate::check_string(new_name),
    checkmate::check_logical(new_name, len = 1)
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

  raster_list <- c()
  if (! var_names %>% is.null()){
    raster_list <- to_merge_area %>%
      fs::dir_ls(type = "file") %>%
      purrr::keep(~ .x %>% stringr::str_detect(stringr::fixed(var_names, ignore_case = T)) %>% any()) %>%
      as.vector()
  }

  if (raster_list %>% length() != var_names %>% length() || raster_list %>% is.null()){
    stop("At least one variable name is ambiguous. Try to use more specific variable names.")
  }

  raster_stack <- raster_list %>%
    raster::stack()

  withr::with_message_sink(
    tempfile(),
    {
      result_crs <- try(raster_stack %>% raster::crs(), silent = T)
    }
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
    a = ATTR_CONTROL_NAMES$cell_id,
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

  shp_grid@data[ATTR_CONTROL_NAMES$cell_id] <- 1:length(grid_cells)
  shp_grid %>%
    sp::spChFIDs(1:length(grid_cells) %>% as.character())

  withr::with_message_sink(
    tempfile(),
    {
      shp_grid@data <- shp_grid@data %>% bind_cols(
        raster_reescaled_countour_masked %>%
          raster::as.list() %>%
          purrr::map_dfc(~ .x %>% raster::values() %>% purrr::discard(is.na) %>% as.data.frame()) %>%
          dplyr::rename_all(~ (var_names %>% unlist()))
      )
    }
  )

  an_area$study_area <- shp_grid

  if (checkmate::test_logical(new_name, len = 1)){
    if (new_name) {
      an_area$name <- an_area$name %>%
        paste0("_", to_merge_area %>% fs::path_file())
    }
  } else if (checkmate::test_string(new_name)){
    an_area$name <- new_name
  }

  an_area %>%
    return()
}
