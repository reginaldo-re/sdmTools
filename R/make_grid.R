#' Make a grid over a study area.
#'
#' @param an_area A \code{SDM_area} object representing the area of the study.
#' @param var_names A list of variable names to keep on cells. It try to match each variable name
#' (ignoring case and partially matched) in the study area. Variables are calculated using
#' the average of features (polygons or lines) coverage by each cell.
#' @param centroid A boolean indicating if x_centroid and y_centroid variables must be computed and appended
#' @param new_name A name to new area study after make a grid over area.
#' @return A \code{SDM_area} object with cells covering the study area. The dataframe contains the variables
#' matched and computed acoording to each cell.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("vect_files/brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = F
#' )
#' SLDF <- readOGR(
#'    system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools"),
#'    layer = "hydro_uper_prpy",
#'    verbose = F
#  )
#'
#' gridded_area <- SPDF %>%
#'  sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
#'  make_grid(var_names = list(), new_name = T)
#'
#' plot(gridded_area)
#'
#' gridded_area <- SLDF %>%
#'   sdm_area("Test area", "EPSG:6933", c(10000, 10000)) %>%
#'   make_grid(var_names = list("Length", "xxx", "Main_ri"))
#'
#' plot(gridded_area)
#' }
make_grid <- function(an_area = NULL, var_names = NULL, new_name = NULL, dir_path = NULL){
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )
  assert_string(new_name, min.chars = 1, null.ok = T)
  assert_string(dir_path, min.chars = 1, null.ok = T)

  if (an_area$gridded){
    "Nothing to do, the grid over study area already exists." %>%
      warn()
    return(an_area)
  }

  UseMethod("make_grid", an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names = NULL, new_name = NULL, dir_path = NULL){
  assert(
    check_class(an_area$study_area, "SpatialPolygons"),
    check_class(an_area$study_area, "SpatialLines")
  )

  an_area$gridded <- T
  if (an_area$study_area %>% is("SpatialPolygons")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialPolygons(
        var_names = var_names,
        resolution = an_area$resolution
      )
  } else if (an_area$study_area %>% is("SpatialLines")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialLines(
        var_names = var_names,
        resolution = an_area$resolution
      )
  }

  an_area <- an_area %>%
    save_gpkg(
      new_name = new_name,
      dir_path = dir_path
    )

  return(an_area)
}

#' @noRd
#' @keywords internal
.make_grid_SpatialPolygons <- function(an_area = NULL, var_names = NULL, resolution = NULL){
  an_area %>%
    assert_class(
      classes = "SpatialPolygons",
      msg = "A modeling area (an_area) must be an object of SpatialPolygons* class."
    )

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- an_area@data %>%
    select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% any_of())) %>%
    rowid_to_column(ATTR_CONTROL_NAMES$cell_id)

  return(
    an_area %>%
      .sp_make_grid(
        resolution = resolution,
        var_names = var_names
      )
  )
}


#' @noRd
#' @keywords internal
.make_grid_SpatialLines <- function(an_area = NULL, var_names = NULL, resolution = NULL){
  an_area %>%
    assert_class(
      classes = "SpatialLines",
      msg = "A modeling area (an_area) must be an object of SpatialLines* class."
    )

  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- an_area@data %>%
    select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% any_of())) %>%
    rowid_to_column(ATTR_CONTROL_NAMES$cell_id)

  return(
    an_area %>%
      .sp_make_grid(
        resolution =  resolution,
        var_names = var_names
      )
  )
}

#' @noRd
#' @keywords internal
.sp_make_grid <- function(an_area = NULL, var_names = NULL, resolution = NULL){
  assert(
    check_class(an_area, "SpatialPolygons"),
    check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  resolution %>%
    assert_number(
      lower = 0.0001,
      msg = "A modeling area (an_area) must have a resolution (resolution) expressed according to the EPSG code of the area."
    )
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )

  var_found <- an_area %>%
    detect_vars(var_names)

  var_not_found <- var_names %>%
    setdiff(var_found) %>%
    unlist(recursive = T)

  if (test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
    c(
      "Variables not found:",
      var_not_found
    ) %>%
      abort()
  }


  if (var_found %>% length() > 0){
    if (!ATTR_CONTROL_NAMES$cell_id %>% is_in(var_found)){
      var_found <- c(ATTR_CONTROL_NAMES$cell_id, var_found)
    }

    an_area@data <- an_area@data %>%
      select(var_found %>% matches()) %>%
      set_names(var_found)
  } else{
    an_area@data <- an_area@data %>%
      select(ATTR_CONTROL_NAMES$cell_id)
  }


  shp_area_bkp <- an_area

  shp_tmp_file <- tempfile()

  an_area %>%
    save_gpkg(
      new_name = shp_tmp_file %>% path_file(),
      dir_path = shp_tmp_file %>% path_dir()
    )

  raster_tmp_file <- tempfile() %>%
    paste0(".tif")

  raster_area <- an_area %>%
    raster(vals=0) %>%
    writeRaster(
      filename = raster_tmp_file,
      format = "GTiff",
      bylayer = F,
      options = c("dstnodata =-9999.0"),
      overwrite=T
    )

  shp_grid <- gdal_rasterize(
    src_datasource = shp_tmp_file  %>% paste0(".gpkg"),
    dst_filename = raster_tmp_file,
    burn = 0,
    at = T,
    co = c("BIGTIFF=YES"),
    a_nodata = "-9999.0",
    tr = c(resolution, resolution),
    tap = T,
    ot = 'Float32',
    output_Raster = T,
    te = an_area %>% bbox() %>% as("vector"),
    verbose = F
  ) %>%
    rasterToPolygons()
  # https://gis.stackexchange.com/questions/166753/fastest-way-to-convert-big-raster-to-polyline-using-r-or-python/313550#313550
  # https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r/357792#357792

  crs(shp_grid) <- crs(an_area)

  grid_cells <- shp_grid %>%
    gIntersects(
      spgeom2 = shp_area_bkp,
      byid = TRUE,
      prepared = T,
      returnDense = F
    ) %>%
    compact()

  shp_grid <- shp_grid[names(grid_cells) %>% as.integer(),] %>%
    spChFIDs(1:length(grid_cells) %>% as.character())

  shp_grid@data <- grid_cells %>%
    map_dfr(as_tibble, .id="grid_cell_id") %>%
    rename(!!ATTR_CONTROL_NAMES$cell_id := value) %>%
    left_join(shp_area_bkp@data, by = ATTR_CONTROL_NAMES$cell_id) %>%
    select(-(!!ATTR_CONTROL_NAMES$cell_id)) %>%
    group_by(grid_cell_id) %>%
    summarise_all(~ ifelse(is.numeric(.), mean(.), .), na.rm = TRUE) %>%
    ungroup() %>%
    select(-grid_cell_id) %>%
    rowid_to_column(ATTR_CONTROL_NAMES$cell_id)

  an_area <- shp_grid

  centroids <- an_area %>%
    gCentroid(byid = TRUE)

  an_area@data <- an_area@data %>%
    bind_cols(centroids@coords %>% as.data.frame()) %>%
    rename(!!ATTR_CONTROL_NAMES$x_centroid := x, !!ATTR_CONTROL_NAMES$y_centroid := y)

  return(an_area)
}

