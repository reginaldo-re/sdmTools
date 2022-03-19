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
  checkmate::assert(
    checkmate::check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    checkmate::check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )
  checkmate::assert_string(new_name, min.chars = 1, null.ok = T)
  checkmate::assert_string(dir_path, min.chars = 1, null.ok = T)

  UseMethod("make_grid", an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names = NULL, new_name = NULL, dir_path = NULL){
  checkmate::assert(
    checkmate::check_class(an_area$study_area, "SpatialPolygons"),
    checkmate::check_class(an_area$study_area, "SpatialLines")
  )

  if (an_area$gridded){
    "Nothing to do, the grid over study area already exists." %>%
      rlang::warn()
    return(an_area)
  }

  an_area$gridded <- T
  if (!new_name %>% is.null()){
    an_area$sdm_area_name <- new_name
  }

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

  if (dir_path %>% is.null()){
    tmp_dir_path <- tempdir() %>%
      fs::path(stringi::stri_rand_strings(1,6))
    quiet(
      tmp_dir_path %>%
        fs::dir_create()
    )
    checkmate::assert_directory_exists(tmp_dir_path)

    if (an_area$dir_path %>% fs::dir_exists()){
      an_area$dir_path %>%
        fs::dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        fs::dir_create()
    )
    checkmate::assert_directory_exists(an_area$dir_path)

    an_area %>%
      save_gpkg()

    tmp_dir_path %>%
      fs::dir_copy(an_area$dir_path, overwrite = T)
    tmp_dir_path %>%
      fs::dir_delete()
  } else {
    an_area$dir_path <- dir_path
    if (an_area$dir_path %>% fs::dir_exists()){
      an_area$dir_path %>%
        fs::dir_delete()
    }
    quiet(
      an_area$dir_path %>%
        fs::dir_create()
    )
    an_area %>%
      save_gpkg()
  }

  return(an_area)
}

#' @noRd
#' @keywords internal
.make_grid_SpatialPolygons <- function(an_area = NULL, var_names = NULL, resolution = NULL){
  checkmate::assert_class(an_area, "SpatialPolygons")

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- an_area@data %>%
    dplyr::select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% tidyselect::any_of())) %>%
    tibble::rowid_to_column(ATTR_CONTROL_NAMES$cell_id)

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
  checkmate::check_class(an_area, "SpatialLines")

  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- an_area@data %>%
    select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% tidyselect::any_of()))

  if (an_area@data %>% names() %>% length()==0){
    an_area@data <- list(
        dummy=1:(an_area@lines %>% length())
      ) %>%
      as.data.frame() %>%
      setNames(c(ATTR_CONTROL_NAMES$cell_id))
  } else{
    an_area@data <- an_area@data %>%
      tibble::rowid_to_column(ATTR_CONTROL_NAMES$cell_id)
  }

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
  grid_cell_id <- value <- x <- y <- NULL

  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  checkmate::assert_number(resolution, lower = 0.0001)
  checkmate::assert(
    checkmate::check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    checkmate::check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )

  var_found <- an_area %>%
    detect_vars(var_names)

  var_not_found <- var_names %>%
    setdiff(var_found)

  if (checkmate::test_character(var_not_found, any.missing = F, all.missing = F, min.len = 1, unique = T)){
    c(
      "Variables not found:",
      var_not_found
    ) %>%
      rlang::abort()
  }


  if (var_found %>% length() > 0){
    if (!ATTR_CONTROL_NAMES$cell_id %>% magrittr::is_in(var_found)){
      var_found <- c(ATTR_CONTROL_NAMES$cell_id, var_found)
    }

    an_area@data <- an_area@data %>%
      dplyr::select(tidyselect::matches(var_found)) %>%
      magrittr::set_names(var_found)
  } else{
    an_area@data <- an_area@data %>%
      dplyr::select(ATTR_CONTROL_NAMES$cell_id)
  }


  shp_area_bkp <- an_area

  shp_tmp_file <- tempfile() %>%
    paste0(".gpkg")

  an_area %>%
    save_gpkg(
      new_name = shp_tmp_file %>% fs::path_file(),
      dir_path = shp_tmp_file %>% fs::path_dir()
    )

  raster_tmp_file <- tempfile() %>%
    paste0(".tif")

  raster_area <- an_area %>%
    raster::raster(vals=0) %>%
    raster::writeRaster(
      filename = raster_tmp_file,
      format = "GTiff",
      bylayer = F,
      options = c("dstnodata =-9999.0"),
      overwrite=T
    )

  shp_grid <- gdalUtils::gdal_rasterize(
    src_datasource = shp_tmp_file,
    dst_filename = raster_tmp_file,
    burn = 0,
    at = T,
    co = c("BIGTIFF=YES"),
    a_nodata = "-9999.0",
    tr = c(resolution, resolution),
    tap = T,
    ot = 'Float32',
    output_Raster = T,
    te = an_area %>% raster::bbox() %>% as("vector"),
    verbose = F
  ) %>%
    raster::rasterToPolygons()
  # https://gis.stackexchange.com/questions/166753/fastest-way-to-convert-big-raster-to-polyline-using-r-or-python/313550#313550
  # https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r/357792#357792

  raster::crs(shp_grid) <- raster::crs(an_area)

  grid_cells <- shp_grid %>%
    rgeos::gIntersects(
      spgeom2 = shp_area_bkp,
      byid = TRUE,
      prepared = T,
      returnDense = F
    ) %>%
    purrr::compact()

  shp_grid <- shp_grid[names(grid_cells) %>% as.integer(),] %>%
    sp::spChFIDs(1:length(grid_cells) %>% as.character())

  shp_grid@data <- grid_cells %>%
    purrr::map_dfr(dplyr::as_tibble, .id="grid_cell_id") %>%
    dplyr::rename(!!ATTR_CONTROL_NAMES$cell_id := value) %>%
    dplyr::left_join(shp_area_bkp@data, by = ATTR_CONTROL_NAMES$cell_id) %>%
    dplyr::select(-(!!ATTR_CONTROL_NAMES$cell_id)) %>%
    dplyr::group_by(grid_cell_id) %>%
    dplyr::summarise_all(~ ifelse(is.numeric(.), mean(.), .), na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grid_cell_id) %>%
    tibble::rowid_to_column(ATTR_CONTROL_NAMES$cell_id)

  an_area <- shp_grid

  centroids <- an_area %>%
    rgeos::gCentroid(byid = TRUE)

  an_area@data <- an_area@data %>%
    dplyr::bind_cols(centroids@coords %>% as.data.frame()) %>%
    dplyr::rename(!!ATTR_CONTROL_NAMES$x_centroid := x, !!ATTR_CONTROL_NAMES$y_centroid := y)

  return(an_area)
}

#' @noRd
#' @keywords internal
.select_vars <- function(a_df = NULL, var_names = NULL){
  checkmate::assert_data_frame(a_df)
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, unique = T)
  )

  a_df <- a_df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.numeric)
    )

  if (! var_names %>% is.null()){
    var_names <- c(ATTR_CONTROL_NAMES$cell_id) %>%
      purrr::prepend(var_names)

    if (var_names %>% length() > 0){
      a_df <- a_df %>%
        dplyr::select(var_names %>% unlist() %>% dplyr::contains())
      #dplyr::rename_all(tolower) %>%
      #dplyr::select(var_names %>% tolower() %>% any_of())

      var_names <- a_df %>%
        names() %>%
        map_chr(~ unlist(var_names[stringr::str_detect(.x, stringr::fixed(var_names %>% unlist(), ignore_case=T))]))

      #var_names <- (var_names %>% unlist())[var_names %>% tolower() %in% (a_df %>% names() %>% tolower())]
      #var_names <- a_df %>% names() %>% map_chr(~ stringr::str_subset(var_names, stringr::fixed(., ignore_case=T)))
      names(a_df) <- var_names
    }
  }
  return(a_df)
}
