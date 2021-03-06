#' Merge rasters over a gridded study area.
#'
#' @param an_area A \code{SDM_area} object with cells covering the study area.
#' @param var_names A list of variable names to keep on cells. It try to match each variable name
#' (ignoring case and partially matched) in the study area. Variables are calculated using
#' the average of features (polygons or lines) coverage by each cell.
#' @param to_merge_area A path to a \code{Raster*} (\url{https://cran.r-project.org/web/packages/raster/})
#' object (folder or file) with variables to merge with.
#' @param sdm_area_name A name to new area study after merge rasters over area.
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
#'  make_grid(var_names = list(), sdm_area_name = T)
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
merge_area <- function(an_area = NULL, to_merge_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL){
  assert(
    nested_check(
      to_merge_area %>%
        check_file_exists(),
      to_merge_area %>%
        check_directory_exists(),
      msg = "An area to be merged (to_merge_area) to a study area (an_area) must be a file or a directory containing vect or raster files."
    ),
    nested_check(
      to_merge_area %>%
        check_class("RasterLayer"),
      to_merge_area %>%
        check_class("RasterStack"),
      to_merge_area %>%
        check_class("RasterBrick"),
      msg = "An area to be merged (to_merge_area) to a study area (an_area) must be an object of class RasterLayer, RasterStack or RasterBrick."
    )
  )
  assert(
    msg = "The variable names argument (var_names) should be NULL to select all available variables or at least one of the following options:",
    var_names %>%
      check_list(
        types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "a vector/list of non duplicated strings to be selected."
      ),
    var_names %>%
      check_character(
        any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "an empty list/vector to select none variable."
      )
  )
  sdm_area_name %>%
    assert_string(min.chars = 1, null.ok = T)
  dir_path %>%
    assert_string(min.chars = 1, null.ok = T)
  an_area$scenarios %>%
    is.null() %>%
    assert_true(
      msg = "It is not possible to merge an area in a sdm_area when scenarios is not null! Please assign null to scenario."
    )

  UseMethod("merge_area", an_area)
}



#' @export
merge_area.SDM_area <- function(an_area = NULL, to_merge_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL){
  if (!an_area$gridded){
    an_area <- an_area %>%
      make_grid.SDM_area(
        var_names = var_names
      )
  }

  an_area$study_area <- an_area$study_area %>%
    .sp_merge_area(
      to_merge_area = to_merge_area,
      var_names = var_names,
      resolution = an_area$resolution
    )

  an_area <- an_area %>%
    save_gpkg(
      sdm_area_name = sdm_area_name,
      dir_path = dir_path
    )

  return(an_area)
}

#' @noRd
#' @keywords internal
.sp_merge_area <-function(an_area = NULL, to_merge_area = NULL, var_names = NULL, resolution = NULL){
  assert(
    an_area %>%
      check_class(
        "SpatialPolygonsDataFrame",
        msg = "A study area (an_area) must be an object of SpatialPolygonsDataFrame class."
      ),
    an_area %>%
      check_class(
        "SpatialLinesDataFrame",
        msg = "A study area (an_area) must be an object of SpatialLinesDataFrame class."
      )
  )
  assert(
    nested_check(
      to_merge_area %>%
        check_file_exists(),
      to_merge_area %>%
        check_directory_exists(),
      msg = "An area to be merged (to_merge_area) to a study area (an_area) must be a file or a directory containing vect or raster files."
    ),
    nested_check(
      to_merge_area %>%
        check_class("RasterLayer"),
      to_merge_area %>%
        check_class("RasterStack"),
      to_merge_area %>%
        check_class("RasterBrick"),
      msg = "An area to be merged (to_merge_area) to a study area (an_area) must be an object of class RasterLayer, RasterStack or RasterBrick."
    )
  )
  assert(
    msg = "The variable names argument (var_names) should be NULL to select all available variables or at least one of the following options:",
    var_names %>%
      check_list(
        types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "a vector/list of non duplicated strings to be selected."
      ),
    var_names %>%
      check_character(
        any.missing = F, all.missing = T, unique = T, null.ok = T,
        msg = "an empty list/vector to select none variable."
      )
  )
  resolution %>%
    assert_number(
      lower = 0.0001,
      msg = "A study area (an_area) must have a resolution (resolution) expressed according to the EPSG code of the area."
    )

  var_found <- to_merge_area %>%
    detect_vars(var_names)

  var_found %>%
    is_empty() %>%
    assert_false(
      msg = "None variable found in to_merge_area."
    )

  var_not_found <- var_names %>%
    setdiff(var_found) %>%
    unlist(recursive = T)

  var_not_found %>%
    is_empty() %>%
    assert_true(
      msg = c("Variables not found:", var_not_found)
    )

  var_conflicted <- var_found %>%
    extract(
      an_area %>%
        detect_vars() %>%
        is_in(var_found)
    )
  var_conflicted %>%
    is_empty() %>%
    assert_true(
      msg = c("These variables already exists in SDM_area object:", var_conflicted)
    )

  if (!to_merge_area %>% is_dir()){
    to_merge_area <- to_merge_area %>%
      path_dir()
  }

  var_names <- var_found
  raster_list <- c()
  if (! var_names %>% is.null()){
    raster_list <- to_merge_area %>%
      dir_ls(type = "file") %>%
      keep(~ .x %>% str_detect(fixed(var_names %>% unlist(), ignore_case = T)) %>% any()) %>%
      as.vector()
  }

  assert(
    (raster_list %>% length() != var_names %>% length()) %>%
      check_false(),
    raster_list %>%
      is.null() %>%
      check_false(),
    combine = "and",
    msg = "At least one variable name is ambiguous. Try to use more specific variable names."
  )
  #if (raster_list %>% length() != var_names %>% length() || raster_list %>% is.null()){
  #  "At least one variable name is ambiguous. Try to use more specific variable names." %>%
  #    abort()
  #}

  raster_stack <- raster_list %>%
    stack()

  result_crs <- quiet(raster_stack %>% crs())

  if (result_crs %>% class() == "try-error" || result_crs %>% is.na()){
    crs(raster_stack) <- crs("EPSG:4326")
    "EPSG code not found in to_merge_area. Atributing EPSG:4326 to to_merge_area." %>%
      inform()
  }

  raster_tmp_file <- tempfile() %>%
    paste0(".tif")

  raster_stack %>%
    writeRaster(
      filename = raster_tmp_file,
      format = "GTiff",
      bylayer = F, #bylayer = T,
      #suffix = lista_rasters_bio %>% names(),
      options = c("dstnodata =-9999.0"),
      overwrite=T,
    )

  shp_countour_file <- tempfile()

  an_area %>%
    aggregate(dissolve = T) %>%
    #gBuffer(width=-(min(c(cell_width, cell_height)))/10, capStyle = "SQUARE", joinStyle = "BEVEL") %>%
    #gUnionCascaded() %>%
    as("SpatialPolygonsDataFrame") %>%
    save_gpkg(
      sdm_area_name = shp_countour_file %>% path_file(),
      dir_path = shp_countour_file %>% path_dir()
    )

  shp_area_file <- tempfile()

  an_area %>%
    save_gpkg(
      sdm_area_name = shp_area_file %>% path_file(),
      dir_path = shp_area_file %>% path_dir()
    )

  raster_file_reescaled_countour <- tempfile() %>%
    paste0(".tif")

  raster_reescaled_countour <- gdalwarp(
    srcfile = raster_tmp_file,
    dstfile = raster_file_reescaled_countour,
    s_srs = crs(raster_stack),
    t_srs = crs(an_area),
    cutline = shp_countour_file %>% paste0(".gpkg"),
    crop_to_cutline = T,
    r = 'average',
    tr = c(resolution, resolution),
    tap = T,
    te = an_area %>% bbox() %>% as("vector"),
    te_srs = crs(an_area),
    dstnodata = "-9999.0",
    ot = 'Float32',
    co = c("BIGTIFF=YES"), #"COMPRESS=DEFLATE", "PREDICTOR=2","ZLEVEL=9"),
    #wo = c("CUTLINE_ALL_TOUCHED=TRUE"),
    multi = T,
    output_Raster = T,
    overwrite = T,
    verbose = F
  ) %>%
    crop(an_area)

  raster_reescaled_countour_masked <- raster_reescaled_countour %>%
    rast()

  quiet(
    raster_reescaled_countour_masked <- raster_reescaled_countour_masked %>%
      mask(an_area %>% vect(), touches=F) %>%
      stack()
  )

  raster_grid <- gdal_rasterize(
    src_datasource = shp_area_file %>% paste0(".gpkg"),
    dst_filename = tempfile() %>% paste0(".tif"),
    #burn = 0,
    a = ATTR_CONTROL_NAMES$cell_id,
    #at = T,
    co = c("BIGTIFF=YES"),
    a_nodata = "-9999.0",
    tr = c(resolution, resolution),
    #tap= T,
    ot = 'Float32',
    output_Raster = T,
    te = an_area %>% bbox() %>% as("vector"),
    verbose = F
  )

  raster_grid <- raster_grid %>%
    rast() %>%
    mask(
      raster_reescaled_countour_masked %>%
        rast() %>%
        extract2(1)
    )

  grid_cells <- raster_grid %>%
    as.vector() %>%
    discard(is.na)

  an_area@data <- an_area@data %>%
    as.data.frame()

  shp_grid <- an_area[grid_cells %>% as.integer(),]

  shp_grid@data[ATTR_CONTROL_NAMES$cell_id] <- 1:length(grid_cells)
  shp_grid %>%
    spChFIDs(1:length(grid_cells) %>% as.character())

  quiet(
      shp_grid@data <- shp_grid@data %>% bind_cols(
        raster_reescaled_countour_masked %>%
          as.list() %>%
          map_dfc(~ .x %>% values() %>% discard(is.na) %>% as.data.frame()) %>%
          rename_all(~ (var_names %>% unlist())),
      )
  )

  an_area <- shp_grid

  return(an_area)
}
