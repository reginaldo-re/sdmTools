globalVariables(c("%>%", "is"))

#' Creates a Study Area
#'
#' @param an_area A sp object, commonly a shapefile,  or a SDM_area object representing the area of study.
#'
#' @return An object representing a study area containing a sp object. If polygons are invalid because topological
#' erros they are corrected. Furthermore, if no coordinate reference system are used it will be set to EPSG:4326.
#' @export
#'
#' @examples
#' \dontrun{
#' main <-  cbind(
#' c(0, 0, 1, 1),
#' c(0, 1, 1, 0)
#' )
#' secondary <- cbind(
#'   c(1, 1.3, 1.3, 1),
#'   c(1, 1.0, 0.7, 0.7)
#' )
#' hole <- main/3 + 1/3
#' island = cbind(
#'   c(1.05, 1.05, 1.55, 1.55),
#'   c(0, .5, .5, 0)
#' )
#'
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
#' SP = SpatialPolygons(list(P))
#' crs(SP) <- CRS("EPSG:6933")
#'
#' new_sdm_area <- sdm_area(SP)
#'
#' class(new_sdm_area)
#' }
sdm_area <- function(an_area=NULL){
  if (!(an_area %>% is("Spatial"))){
    stop("Study area must be in a vectorized format!")
  }

  if (an_area %>% raster::crs() %>% is.na()){
    raster::crs(an_area) <- sp::CRS("EPSG:4326")
  }

  if (an_area %>% is("SpatialPolygonsDataFrame") || an_area %>% is("SpatialPolygons")) {
    suppressWarnings(
      if (!an_area %>% rgeos::gIsValid()){
          an_area <- an_area %>%
            rgeos::gBuffer(byid=TRUE, width=0)
      }
    )
  }

  sdm_area_tmp <- list(
    study_area=an_area
  )

  return (
    structure(
      sdm_area_tmp,
      class= "SDM_area"
    )
  )
}

.sf_areas_gt <- function(an_area, lower_bound) {
  if (!(lower_bound %>% is.numeric()) || lower_bound <= 0) {
    stop("Invalid lower bound!")
  }
  if (an_area %>% raster::crs() %>% is.na()){
    raster::crs(an_area) <- sp::CRS("EPSG:4326")
  }
  suppressWarnings(
    vect_tmp <- an_area %>%
      rgeos::gBuffer(width = 0) %>%
      sp::disaggregate()
  )

  if (is.na(raster::crs(vect_tmp))){
    remain_areas <- vect_tmp[rgeos::gArea(vect_tmp, byid = T) > lower_bound, ]
  }
  else {
    remain_areas <- vect_tmp[raster::area(vect_tmp) > lower_bound, ]
  }

  if (length(remain_areas@polygons)>0){
    remain_areas <- an_area %>%
      raster::disaggregate() %>%
      rgeos::gIntersection(remain_areas, byid = T)
  }

  return(remain_areas)
}

#' Drop noncontiguous polygons with an area smaller or equal lower_bound.
#'
#' @param an_area A sp object, commonly a shapefile,  or a SDM_area object representing the area of study.
#' @param lower_bound A lower bound area indicating polygons which it going to dropped out.
#'
#' @return A sp object with remaining disaggregated polygons with area greater than lower_bound, or a SDM_area
#' containing a sp object with remaining disaggregated polygons with area greater than lower_bound.
#' Furthermore, if no coordinate reference system are used it will be set to EPSG:4326.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
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
#'
#' plot(SP)
#' }
areas_gt <- function(an_area, lower_bound = 0){
  UseMethod("areas_gt")
}

#' @export
areas_gt.default <- function(an_area, lower_bound = 0) {
    warning("Nothing to do, the type of an_area must be: SpatialPolygons, SpatialPolygonsDataFrame, or SDM_area.")
    return(an_area)
}

#' @export
areas_gt.SpatialPolygons <- function(an_area, lower_bound = 0) {
  an_area %>%
    .sf_areas_gt(lower_bound) %>%
    return()
}

#' @export
areas_gt.SpatialPolygonsDataFrame <- function(an_area, lower_bound = 0) {
  an_area %>%
    .sf_areas_gt(lower_bound) %>%
    return()
}

#' @export
areas_gt.SDM_area <- function(an_area, lower_bound = 0) {
  an_area$study_area <- an_area$study_area %>%
    .sf_areas_gt(lower_bound)
  return(an_area)
}

#' Make a grid over study area.
#'
#' @param an_area A sp object, commonly a shapefile,  or a SDM_area object representing the area of study.
#' @param cell_width The width of cells.
#' @param cell_height The height of cells.
#' @param var_names A list (or vector) of variable names to keep on cells. Variables area computed using
#' the average of features (polygons or lines) that over each cell. It try to match each variable name
#' (ignoring case) in the study area.
#' @param centroid A boolean indicating if x_centroid and y_centroid variables must be computed and appended
#' to variables./
#'
#' @return A SpatialPolygonsDataFrame with cells covering the study area. The dataframe contains the variables
#'  matched e computed acoording to each cell.
#' @export
#'
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
#'
#' gridded_area <- SLDF %>%
#'    make_grid(cell_width = 50000, cell_height = 50000, var_names = c("Length", "xxx", "Main_ri"), centroid=T)
#'
#' gridded_area %>% plot()
#'
#' gridded_area <- SPDF %>%
#'    make_grid(cell_width = 50000, cell_height = 50000, var_names = c("geocodigo"), centroid=T)
#'
#' gridded_area %>% plot()
#'
#' new_sdm_area <- sdm_area(SPDF)
#'
#' gridded_area < new_sdm_area %>%
#'    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)
#'
#' gridded_area %>% plot()
#' }
make_grid <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  UseMethod("make_grid")
}


#' @export
make_grid.default <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  warning("Nothing to do, the type of an_area must be: SpatialPolygons, SpatialPolygonsDataFrame, SpatialLines, SpatialLinesDataFrame or SDM_area.")
  return(an_area)
}


#' @export
make_grid.SpatialPolygons <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  an_area <- an_area %>%
    #raster::buffer(
    #  (c(cell_width, cell_height) %>% min()),
    #  dissolve=T
    #) %>%
    rgeos::gBuffer(width=0, byid=T) %>%
    as("SpatialPolygonsDataFrame")

  an_area@data <- an_area@data %>%
    select(-(c("dummy", "cell_id") %>% any_of())) %>%
    rowid_to_column("cell_id")

  an_area %>%
    .make_grid_sp(cell_width, cell_height, var_names, centroid) %>%
    return()
}

#' @export
make_grid.SpatialLines <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  an_area <- an_area %>%
    as("SpatialLinesDataFrame")

  an_area@data <- an_area@data %>%
    select(-(c("cell_id") %>% any_of()))

  if (an_area@data %>% names() %>% length()==0){
    an_area@data <- data.frame(
      list(
        cell_id=1:(an_area@lines %>% length())
      )
    )
  } else{
    an_area@data <- an_area@data %>%
      rowid_to_column("cell_id")
  }

  an_area %>%
    .make_grid_sp(cell_width, cell_height, var_names, centroid) %>%
    return()
}

#' @export
make_grid.SDM_area <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  an_area$study_area <- an_area$study_area %>%
    make_grid(cell_width, cell_height, var_names, centroid) %>%
  return(an_area)
}

.select_vars <- function(a_df, var_names=NULL){
  a_df <- a_df %>%
    mutate(across(where(is.factor), as.numeric))

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

.make_grid_sp <- function(an_area, cell_width=0, cell_height=0, var_names=NULL, centroid=T){
  if (cell_width<=0 || cell_height<=0){
    stop("Invalid cell_width or cell_height.")
  }

  an_area@data <- an_area@data %>%
    .select_vars(var_names)

  shp_area_bkp <- an_area

  shp_tmp_file <- tempfile() %>% paste0(".shp")
  an_area %>%
    rgdal::writeOGR(
      dsn = shp_tmp_file,
      layer=".",
      driver="ESRI Shapefile",
      overwrite=T
    )

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
    tr = c(cell_width, cell_height),
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
    purrr::map_dfr(as_tibble, .id="grid_cell_id") %>%
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



