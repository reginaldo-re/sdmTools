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
#' SPDF <- rgdal::readOGR(
#'    system.file("vect_files/brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = F
#' )
#' SLDF <- rgdal::readOGR(
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
make_grid <- function(an_area = NULL, var_names = NULL, new_name = F, centroid = T){
  UseMethod("make_grid", an_area)
}

#' @export
make_grid.default <- function(an_area = NULL, var_names = NULL, new_name = F, centroid = T){
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names=NULL, new_name = F, centroid = T){
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

  an_area$attr_control_name <- create_enum(dummy, cell_id, x_centroid, y_centroid)

  if (an_area$study_area %>% is("SpatialPolygons")){
    an_area$study_area <- an_area$study_area %>%
      as("SpatialPolygonsDataFrame")

    an_area$study_area@data <- an_area$study_area@data %>%
      dplyr::select(-(an_area$attr_control_name %>% as_vector() %>% tidyselect::any_of())) %>%
      tibble::rowid_to_column(an_area$attr_control_name$cell_id)
  } else if (an_area$study_area %>% is("SpatialLines")){
    an_area$study_area <- an_area$study_area %>%
      as("SpatialLinesDataFrame")

    an_area$study_area@data <- an_area$study_area@data %>%
      select(-(an_area$attr_control_name %>% as_vector() %>% tidyselect::any_of()))

    if (an_area$study_area@data %>% names() %>% length()==0){
      an_area$study_area@data <- list(
          dummy=1:(an_area$study_area@lines %>% length())
        ) %>%
        as.data.frame() %>%
        setNames(c(an_area$attr_control_name$cell_id))

    } else{
      an_area$study_area@data <- an_area$study_area@data %>%
        tibble::rowid_to_column(an_area$attr_control_name$cell_id)
    }
  }

  an_area$study_area <- an_area %>%
    .sp_make_grid(an_area$resolution, var_names, new_name, centroid)

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

.sp_make_grid <- function(an_area = NULL, a_res = NULL, var_names = NULL, new_name = F, centroid = T){
  grid_cell_id <- value <- x <- y <- NULL

  a_sdm_area <- an_area
  an_area <- an_area$study_area

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
    dplyr::rename(!!a_sdm_area$attr_control_name$cell_id := value) %>%
    dplyr::left_join(shp_area_bkp@data, by = a_sdm_area$attr_control_name$cell_id) %>%
    dplyr::select(-(!!a_sdm_area$attr_control_name$cell_id)) %>%
    dplyr::group_by(grid_cell_id) %>%
    dplyr::summarise_all(~ ifelse(is.numeric(.), mean(.), .), na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grid_cell_id) %>%
    tibble::rowid_to_column(a_sdm_area$attr_control_name$cell_id)

  an_area <- shp_grid

  if (centroid){
    centroids <- an_area %>%
      rgeos::gCentroid(byid=TRUE)

    an_area@data <- an_area@data %>%
      dplyr::bind_cols(centroids@coords %>% as.data.frame()) %>%
      dplyr::rename(!!a_sdm_area$attr_control_name$x_centroid := x, !!a_sdm_area$attr_control_name$y_centroid := y)
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


.remove_attr_control <- function(names = NULL){

}
