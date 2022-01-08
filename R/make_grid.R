#' Make a grid over study area.
#'
#' @param an_area A SDM_area object representing the area of study.
#' @param var_names A list of variable names to keep on cells. Variables area computed using
#' the average of features (polygons or lines) that over each cell. It try to match each variable name
#' (ignoring case) in the study area.
#' @param centroid A boolean indicating if x_centroid and y_centroid variables must be computed and appended
#' to variables./
#' @return A SDM_area  object with cells covering the study area. The dataframe contains the variables
#'  matched and computed acoording to each cell.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#'  )
#' SLDF <- readOGR(
#'    system.file("hydro_uper_prpy.gpkg", package="sdmTools"),
#'    layer = "hydro_uper_prpy",
#'    verbose = FALSE
#'  )
#' new_sdm_area <- sdm_area(SPDF, "EPSG:6933", c(50000, 50000)))
#' gridded_area <- make_grid(
#'       new_sdm_area,
#'       var_names = c("Length", "xxx", "Main_ri"),
#'       centroid = T
#'  )
#' gridded_area %>% plot()
#'
#' new_sdm_area <- sdm_area(SLDF, "EPSG:6933", c(50000, 50000)))
#' gridded_area <- make_grid(
#'       new_sdm_area,
#'       var_names = c("Length", "xxx", "Main_ri"),
#'       centroid = T
#'  )
#' gridded_area %>% plot()
#' }
make_grid <- function(an_area = NULL, var_names = NULL, new_name = F, centroid=T){
  UseMethod("make_grid", an_area)
}

#' @export
make_grid.default <- function(an_area = NULL, var_names = NULL, new_name = F, centroid=T){
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names=NULL, new_name = F, centroid=T){
  checkmate::assert(
    checkmate::check_class(an_area$study_area, "SpatialPolygons"),
    checkmate::check_class(an_area$study_area, "SpatialLines")
  )
  if (an_area$gridded){
    warning("Nothing to do, the grid over study area already exists.")

    an_area %>%
      return()
  }
  checkmate::assert(
    checkmate::check_string(new_name),
    checkmate::check_logical(new_name, len = 1)
  )

  if (an_area$study_area %>% is("SpatialPolygons")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialPolygons(an_area$resolution, var_names, new_name, centroid)
  } else if (an_area$study_area %>% is("SpatialLines")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialLines(an_area$resolution, var_names, new_name, centroid)
  }

  an_area$gridded <- T

  if (checkmate::test_logical(new_name, min.len = 1)){
    if (new_name) {
      an_area$name <- an_area$name %>%
        paste0("_grid")
    }
  } else if (checkmate::test_string(new_name)){
      an_area$name <- new_name
  }

  an_area %>%
    return()
}

.make_grid_SpatialPolygons <- function(an_area = NULL, a_res = NULL, var_names = NULL, new_name = F, centroid=T){
  checkmate::check_class(an_area, "SpatialPolygons")

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- an_area@data %>%
    dplyr::select(-(c("dummy", "cell_id", "x_centroid", "y_centroid") %>% tidyselect::any_of())) %>%
    tibble::rowid_to_column("cell_id")

  an_area %>%
    .sp_make_grid(a_res, var_names, centroid) %>%
    return()
}

.make_grid_SpatialLines <- function(an_area = NULL, a_res = NULL, var_names = NULL, new_name = F, centroid=T){
  checkmate::check_class(an_area, "SpatialLines")

  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- an_area@data %>%
    select(-(c("dummy", "cell_id", "x_centroid", "y_centroid") %>% tidyselect::any_of()))

  if (an_area@data %>% names() %>% length()==0){
    an_area@data <- data.frame(
      list(
        cell_id=1:(an_area@lines %>% length())
      )
    )
  } else{
    an_area@data <- an_area@data %>%
      tibble::rowid_to_column("cell_id")
  }

  an_area %>%
    .sp_make_grid(a_res, var_names, new_name, centroid) %>%
    return()
}

.sp_make_grid <- function(an_area = NULL, a_res = NULL, var_names = NULL, new_name = F, centroid=T){
  cell_id <- grid_cell_id <- value <- x <- y <- NULL
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  checkmate::assert_numeric(a_res, len = 2, lower = 0.0001)
  checkmate::assert(
    checkmate::check_null(var_names),
    checkmate::check_list(var_names, unique = T)
  )
  checkmate::check_logical(centroid, len = 1)

  an_area@data <- an_area@data %>%
    .select_vars(var_names)

  shp_area_bkp <- an_area

  shp_tmp_file <- tempfile() %>%
    paste0(".gpkg")

  an_area %>%
    save_gpkg(shp_tmp_file)

  raster_tmp_file <- tempfile() %>%
    paste0(".tif")

  raster_area <- an_area %>%
    raster::raster(vals=0) %>%
    raster::writeRaster(
      raster_tmp_file,
      format = "GTiff",
      bylayer = F,
      options = c("dstnodata =-9999.0"),
      overwrite=T
    )

  shp_grid <- gdalUtils::gdal_rasterize(
    shp_tmp_file,
    raster_tmp_file,
    burn = 0,
    at = T,
    co = c("BIGTIFF=YES"),
    a_nodata = "-9999.0",
    tr = a_res,
    tap= T,
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
      shp_area_bkp,
      byid = TRUE,
      prepared = T,
      returnDense = F
    ) %>%
    purrr::compact()

  shp_grid <- shp_grid[names(grid_cells) %>% as.integer(),] %>%
    sp::spChFIDs(1:length(grid_cells) %>% as.character())

  shp_grid@data <- grid_cells %>%
    purrr::map_dfr(dplyr::as_tibble, .id="grid_cell_id") %>%
    dplyr::rename(cell_id=value) %>%
    dplyr::left_join(shp_area_bkp@data, by = "cell_id") %>%
    dplyr::select(-cell_id) %>%
    dplyr::group_by(grid_cell_id) %>%
    dplyr::summarise_all(~ ifelse(is.numeric(.), mean(.), .), na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grid_cell_id) %>%
    tibble::rowid_to_column("cell_id")

  an_area <- shp_grid

  if (centroid){
    centroids <- an_area %>%
      rgeos::gCentroid(byid=TRUE)

    an_area@data <- an_area@data %>%
      dplyr::bind_cols(centroids@coords %>% as.data.frame()) %>%
      dplyr::rename(x_centroid = x, y_centroid = y)
  }
  return(an_area)
}


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
    var_names <- c("cell_id") %>%
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
  a_df %>%
    return()
}
