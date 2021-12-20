utils::globalVariables(c("where"))

#' Creates a Study Area
#'
#' @param an_area A sp object or a path to a file, commonly a shapefile or geopackage, representing the area of study.
#' @param a_crs A valid CRS code, for example EPSG:4326.
#' @param a_res A vector containing the resolution of the study area. The format is two numeric values
#' (width and height) according to CRS used. So, the numeric values can be express different types of units of
#' measurement, for example deegres or meters.
#' @return An object representing a study area containing a sp object. Occasional topological errors on polygons of the
#' object are corrected. If the CRS of the study area is different from the CRS passed to the function, the study
#' area is reproject.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#' )
#' new_sdm_area <- sdm_area(SPDF, "EPSG:6933", c(50000, 50000)))
#'
#' class(new_sdm_area)
#' }
#'
sdm_area <- function(an_area = NULL, a_crs = NULL, a_res = NULL){
  UseMethod("sdm_area", an_area)
}

#' @export
sdm_area.SpatialPolygons <- function(an_area = NULL, a_crs = NULL, a_res = NULL){
  if (raster::crs(an_area) %>% is.na()){
    stop("The study area must have a valid CRS.")
  }
  an_area %>%
    .sp_sdm_area(a_crs, a_res) %>%
    return()
}

#' @export
sdm_area.SpatialLines <- function(an_area = NULL, a_crs = NULL, a_res = NULL){
  if (raster::crs(an_area) %>% is.na()){
    stop("The study area must have a valid CRS.")
  }
  an_area %>%
    .sp_sdm_area(a_crs, a_res) %>%
    return()
}


#' @export
sdm_area.character <- function(an_area = NULL, a_crs = NULL, a_res = NULL){
  checkmate::assert_file(an_area)
  an_area %>%
    rgdal::readOGR(verbose = F) %>%
    .sp_sdm_area(a_crs, a_res) %>%
    return()
}
.sp_sdm_area <- function(an_area = NULL, a_crs = NULL, a_res = NULL){
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  checkmate::assert_class(raster::crs(an_area), "CRS")

  checkmate::assert_string(a_crs, min.chars = 6)
  a_crs <- a_crs %>%
    stringr::str_to_upper()
  res_crs <- suppressWarnings(try(raster::crs(a_crs)))
  if (res_crs %>% is("try-error")){
    stop("Invalid CRS.")
  }


  checkmate::assert_numeric(a_res, len = 2, lower = 0.0001)

  an_area <- an_area %>%
    sp::spTransform(a_crs %>% raster::crs()) %>%
    repair_area()

  sdm_area_tmp <- list(
    crs = a_crs,
    resolution = a_res,
    gridded = .is_gridded(an_area),
    study_area = an_area
  )

  return (
    structure(
      sdm_area_tmp,
      class= "SDM_area"
    )
  )
}

.is_gridded <- function(an_area){
  if (!(an_area %>% is("SpatialPolygonsDataFrame"))) {
    return(F)
  }
  res_summ <- an_area %>%
    raster::area() %>%
    summary()
  return (res_summ["Median"] == res_summ["Mean"] && (an_area@polygons %>% length() > 1))
}

#' Drop noncontiguous polygons with an area smaller or equal lower_bound.
#'
#' @param an_area A SDM_area object representing the area of study.
#' @param lower_bound A lower bound area indicating polygons which it going to dropped out.
#' @return A SDM_area containing a sp object with remaining disaggregated polygons with area greater than lower_bound.
#' @export
#' @examples
#' \dontrun{
#' main <- cbind(
#'   c(0, 0, 1, 1),
#'   c(0, 1, 1, 0)
#' )
#' secondary <- cbind(
#'   c(1, 1.3, 1.3, 1),
#'   c(1, 1.0, 0.7, 0.7)
#' )
#' hole <- main / 3 + 1 / 3
#' island <- cbind(
#'   c(1.05, 1.05, 1.55, 1.55),
#'   c(0, .5, .5, 0)
#' )
#' P <- Polygons(
#'   ID = 1,
#'   list(
#'     Polygon(main),
#'     Polygon(hole, hole = TRUE),
#'     Polygon(island),
#'     Polygon(secondary)
#'   )
#' )
#'
#' SP <- SpatialPolygons(list(P))
#' new_sdm_area <- sdm_area(SP, "EPSG:6933", c(0.1, 0.1)))
#' plot(new_sdm_area)
#' }
areas_gt <- function(an_area = NULL, lower_bound = 0){
  UseMethod("areas_gt")
}

#' @export
areas_gt.default <- function(an_area = NULL, lower_bound = 0) {
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
areas_gt.SDM_area <- function(an_area = NULL, lower_bound = 0) {
  checkmate::check_class(an_area$study_area, "SpatialPolygons")
  an_area$study_area <- an_area$study_area %>%
    .sp_areas_gt(lower_bound)
  return(an_area)
}

.sp_areas_gt <- function(an_area = NULL, lower_bound = 0) {
  checkmate::check_class(an_area, "SpatialPolygons")
  checkmate::assert_numeric(lower_bound, len = 1, lower = 0.0)

  an_area <- an_area %>%
    repair_area()

  an_area_agg <- an_area %>%
    raster::aggregate() %>%
    raster::disaggregate()

  if (is.na(raster::crs(an_area_agg))){
    remain_areas_agg <- an_area_agg[rgeos::gArea(an_area_agg, byid = T) > lower_bound, ]
  }
  else {
    remain_areas_agg <- an_area_agg[raster::area(an_area_agg) > lower_bound, ]
  }

  if ((remain_areas_agg@polygons %>% length())>0){
    an_area %>%
      raster::intersect(remain_areas_agg) %>%
      return()
  } else {
    remain_areas_agg %>%
      return()
  }
}

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
make_grid <- function(an_area = NULL, var_names = NULL, has_centroid=T){
  UseMethod("make_grid", an_area)
}

#' @export
make_grid.default <- function(an_area = NULL, var_names = NULL, has_centroid=T){
  warning("Nothing to do, an_area must be an SDM_area object.")
  return(an_area)
}

#' @export
make_grid.SDM_area <- function(an_area = NULL, var_names=NULL, centroid=T){
  checkmate::assert(
    checkmate::check_class(an_area$study_area, "SpatialPolygons"),
    checkmate::check_class(an_area$study_area, "SpatialLines")
  )
  if (an_area$gridded){
    warning("Nothing to do, the grid over that study area already exists.")
    return(an_area)
  }

  if (an_area$study_area %>% is("SpatialPolygons")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialPolygons(an_area$resolution, var_names, centroid)
  } else if (an_area$study_area %>% is("SpatialLines")){
    an_area$study_area <- an_area$study_area %>%
      .make_grid_SpatialLines(an_area$resolution, var_names, centroid)
  }

  an_area$gridded <- T

  return(an_area)
}

.make_grid_SpatialPolygons <- function(an_area = NULL, a_res = NULL, var_names = NULL, has_centroid=T){
  checkmate::check_class(an_area, "SpatialPolygons")

  an_area <- an_area %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- an_area@data %>%
    dplyr::select(-(c("dummy", "cell_id", "x_centroid", "y_centroid") %>% tidyselect::any_of())) %>%
    tibble::rowid_to_column("cell_id")

  an_area %>%
    .sp_make_grid(a_res, var_names, has_centroid) %>%
    return()
}

.make_grid_SpatialLines <- function(an_area = NULL, a_res = NULL, var_names = NULL, has_centroid=T){
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
    .sp_make_grid(a_res, var_names, has_centroid) %>%
    return()
}

.sp_make_grid <- function(an_area = NULL, a_res = NULL, var_names = NULL, has_centroid=T){
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
  checkmate::check_logical(has_centroid, len = 1)

  an_area@data <- an_area@data %>%
    .select_vars(var_names)

  shp_area_bkp <- an_area

  shp_tmp_file <- tempfile() %>% paste0(".gpkg")
  an_area %>%
    save_gpkg(shp_tmp_file)

  raster_tmp_file <- tempfile() %>% paste0(".tif")
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

  if (has_centroid){
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

  if (!is.null(var_names)){
    var_names <- c("cell_id") %>%
      purrr::prepend(var_names)
    if (length(var_names)>0){
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
    else {
      a_df <- a_df %>%
        dplyr::select(-everything())
    }
  }
  return(a_df)
}


#' Plot a SDM_area
#'
#' @param x SDM_area object
#' @param ... Additional parameters
#'
#' @export
#' @method plot SDM_area
plot.SDM_area <- function(x, ...){
  checkmate::assert(
    checkmate::check_class(x$study_area, "SpatialPolygons"),
    checkmate::check_class(x$study_area, "SpatialLines")
  )
  x$study_area %>% plot(...)
}

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
  return(an_area)
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

  if (((!is.null(var_names) && length(var_names)==0))){
    stop("Nothing to do, It must be exists at least one variable to merge and one valid area source.")
    return(an_area)
  }
  if (is.null(var_names)){
    if (to_merge_area %>% fs::is_file()){
      to_merge_area <- to_merge_area %>% fs::path_dir()
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

  if (length(raster_list)!=length(var_names)){
    stop("At least one variable name is ambiguous. Try to use more specific variable names.")
    return(an_area)
  }

  raster_stack <- raster_list %>%
    raster::stack()

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
    terra::mask((raster_reescaled_countour_masked %>% terra::rast())[[1]])


  grid_cells <- raster_grid %>%
    as.vector() %>%
    purrr::discard(is.na)

  an_area$study_area@data <- an_area$study_area@data %>%
    as.data.frame()

  shp_grid <- an_area$study_area[grid_cells %>% as.integer(),]

  shp_grid@data$cell_id <- 1:length(grid_cells)
  shp_grid %>%
    sp::spChFIDs(as.character(1:length(grid_cells)))

  suppressMessages(
    shp_grid@data <- shp_grid@data %>% bind_cols(
      raster_reescaled_countour_masked %>%
        raster::as.list() %>%
        purrr::map_dfc(~ .x %>% raster::values() %>% purrr::discard(is.na) %>% as.data.frame()) %>%
        dplyr::rename_all(~ (var_names %>% unlist()))
    )
  )
  an_area$study_area <-shp_grid
  return(an_area)
}


#' @export
area_geomap <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  UseMethod("area_geomap", an_area)
}


#' @export
area_geomap.SDM_area <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  an_area$study_area %>%
    .sp_area_geomap(
      title,
      subtitle = ifelse(crs_subtitle==T, paste0(raster::crs(an_area)), ""),
      lat,
      long,
      group,
      colour,
      fill
    )
}


.sp_area_geomap <- function(an_area = NULL, title = "", subtitle = "", lat = "lat", long = "long", group = "group", colour = "black", fill = NA) {
  number_format <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

  if (("cell_id" %in% (an_area %>% names()))){
    map_data <- an_area %>%
      sdm_tidy("cell_id")
  }
  else {
    map_data <- an_area %>%
      sdm_tidy()
  }

  map_tmp <- ggplot2::ggplot(
    data =  map_data,
    ggplot2::aes_string(
      x = long,
      y = lat,
      group = group
    )
  ) +
    ggplot2::scale_x_continuous(labels = number_format) +
    ggplot2::scale_y_continuous(labels = number_format) +
    ggplot2::coord_equal()


  if (title != ""){
    map_tmp <- map_tmp +
      ggplot2::labs(title=title)
  }

  if (subtitle != ""){
    map_tmp <- map_tmp +
      ggplot2::labs(subtitle = subtitle)
  }

  if (is.na(fill)){
    map_tmp <- map_tmp +
      ggplot2::geom_polygon(colour = colour, fill = NA)
  } else {
    map_tmp <- map_tmp +
      ggplot2::geom_polygon(colour = NA, ggplot2::aes_string(fill = fill))
  }

  return(map_tmp)
}


repair_area <- function(an_area = NULL){
  UseMethod("repair_area", an_area)
}

repair_area.default <- function(an_area = NULL){
  an_area %>%
    return()
}

repair_area.SpatialPolygons <- function(an_area = NULL){
  res_crs <- suppressWarnings(try(raster::crs(an_area)))
  if (res_crs %>% is("try-error")){
    stop("Invalid CRS.")
  }
  suppressWarnings(
    an_area %>%
      rgeos::gBuffer(byid=TRUE, width=0) %>%
      return()
  )
}


#' @export
grid_geomap <- function(an_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  UseMethod("grid_geomap", an_area)
}


#' @export
grid_geomap.SDM_area <- function(an_area = NULL, a_gridded_area = NULL, title = "", crs_subtitle = T, lat = "lat", long = "long", group = "group", colour = "black", fill = NA){
  geo_map <- an_area$study_area %>%
    .sp_area_geomap(
      title,
      subtitle = ifelse(crs_subtitle==T, paste0(raster::crs(an_area)), ""),
      lat,
      long,
      group,
      colour,
      fill
    )
  if (!(a_gridded_area %>% is.null())){
    geo_map <- geo_map +
        ggplot2::geom_polygon(data = sdm_tidy(a_gridded_area),
                     ggplot2::aes(x = long, y = lat, group = group),
                     colour = "#4d4d4d",
                     fill = NA)
  }
}
