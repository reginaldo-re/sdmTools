#' @export
sdm_area.Spatial <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  an_area %>%
    .sp_sdm_area(name, epsg_code, a_res) %>%
    return()
}

#' @export
sdm_area.character <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  checkmate::assert(
    checkmate::check_file_exists(an_area),
    checkmate::check_string(an_area, min.chars = 3)
  )

  if (! an_area %>% fs::is_file()){
    an_area_path <- .find_files(an_area)
    if (an_area_path %>% is.null()){
      stop("A study area file not found!")
    }
    else if (an_area_path %>% length() > 1){
      stop(
        paste(
          "The file name of study area is ambiguous:",
          paste(
            an_area_path,
            sep = ", ",
            collapse = " "
          )
        )
      )
    } else {
      an_area <- an_area_path
    }
  }

  an_area %>%
    rgdal::readOGR(verbose = F) %>%
    .sp_sdm_area(name, epsg_code, a_res) %>%
    return()
}

.sp_sdm_area <- function(an_area = NULL, name = NULL, epsg_code = NULL, a_res = NULL){
  checkmate::assert_string(name)
  checkmate::assert(
    checkmate::check_class(an_area, "SpatialPolygons"),
    checkmate::check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  area_crs <- suppressWarnings(try(raster::crs(an_area)))
  if(class(area_crs) == "try-error" || area_crs %>% is.na()){
    raster::crs(an_area) <- raster::crs("EPSG:4326")
  }
  #checkmate::assert_class(result_crs, "CRS", .var.name = "an_area@crs")

  checkmate::assert(
    checkmate::check_null(epsg_code),
    checkmate::check_string(epsg_code, min.chars = 6, fixed = "EPSG:"),
    .var.name = "epsg_code"
  )

  new_crs <- NULL
  if (epsg_code %>% is.null()){
    new_crs <- an_area %>%
      raster::crs()
  } else {
    new_crs <- suppressWarnings(
      try(raster::crs(epsg_code))
    )
    checkmate::assert(
      checkmate::check_class(new_crs, "try-error"),
      checkmate::check_class(new_crs, "CRS"),
      checkmate::check_scalar_na(new_crs),
      .var.name = "epsg_code"
    )
  }

  gridded <- an_area %>%
    .is_gridded()

  if (gridded && a_res %>% is.null()){
    a_res <- an_area %>%
      .get_resolution()
  }
  checkmate::assert_numeric(a_res, len = 2, lower = 0.0001)

  an_area <- an_area %>%
    sp::spTransform(new_crs) %>%
    repair_area()

  sdm_area_tmp <- list(
    name = name,
    crs = new_crs,
    epsg_code = epsg_code,
    resolution = a_res,
    gridded = gridded,
    study_area = an_area
  )

  structure(
    sdm_area_tmp,
    class= "SDM_area"
  ) %>%
    return()
}

.sp_areas_gt <- function(an_area = NULL, lower_bound = 0) {
  checkmate::check_class(an_area, "SpatialPolygons")
  checkmate::assert_numeric(lower_bound, len = 1, lower = 0.0)

  an_area <- an_area %>%
    repair_area()

  an_area_agg <- an_area %>%
    raster::aggregate() %>%
    raster::disaggregate()

  if (an_area_agg %>% raster::crs() %>% is.na()){
    remain_areas_agg <- an_area_agg[rgeos::gArea(an_area_agg, byid = T) > lower_bound, ]
  }
  else {
    remain_areas_agg <- an_area_agg[raster::area(an_area_agg) > lower_bound, ]
  }

  if (remain_areas_agg@polygons %>% length() > 0){
    an_area %>%
      raster::intersect(remain_areas_agg) %>%
      return()
  } else {
    remain_areas_agg %>%
      return()
  }
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

  shp_tmp_file <- tempfile() %>%
    paste0(".gpkg")

  an_area %>%
    save_gpkg(file_name = shp_tmp_file)

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

  if (has_centroid){
    centroids <- an_area %>%
      rgeos::gCentroid(byid=TRUE)

    an_area@data <- an_area@data %>%
      dplyr::bind_cols(centroids@coords %>% as.data.frame()) %>%
      dplyr::rename(x_centroid = x, y_centroid = y)
  }
  return(an_area)
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

  if ((! var_names %>% is.null()) && (var_names %>% length()==0)){
    stop("Nothing to do, it must be exists at least one variable to merge and one valid area source.")
    an_area %>%
      return()
  }
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
    an_area %>%
      return()
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
    save_gpkg(file_name = shp_countour_file)

  shp_area_file <- tempfile() %>%
    paste0(".gpkg")

  an_area$study_area %>%
    save_gpkg(file_name = shp_area_file)

  shp_grid_file <- tempfile() %>%
    paste0(".gpkg")

  an_area$study_area %>%
    as("SpatialPolygonsDataFrame") %>%
    save_gpkg(file_name = shp_grid_file)

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

  map_tmp %>%
    return()
}


repair_area.SpatialPolygons <- function(an_area = NULL){
  res_crs <- suppressWarnings(
    try(raster::crs(an_area))
  )
  if (res_crs %>% is("try-error")){
    stop("Invalid CRS.")
  }
  suppressWarnings(
    an_area %>%
      rgeos::gBuffer(byid=TRUE, width=0) %>%
      return()
  )
}
