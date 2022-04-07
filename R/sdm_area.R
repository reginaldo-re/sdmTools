#' Creates a Study Area
#'
#' A study area (\code{SDm_area}) represents a geographic location delimited by primitive shapes (lines and polygons)  and
#' associated attribute information about characteristics of that location.
#'
#' The attribute information of the study area are constant in time. In contrast, the \code{SDM_area} object
#' has an array attribute storing multiple scenarios, representing attribute information about characteristics of the
#' location that change in time.
#'
#' @param an_area An object of package \code{sp} (<https://cran.r-project.org/web/packages/sp>)
#' or a path to a file, commonly a shapefile or geopackage, representing the area of study.
#' @param name A name do describe the study area.
#' @param epsg_code A valid EPSG code, for example EPSG:4326.
#' @param resolution A vector containing the resolution of the study area. The format is two numeric values
#' (width and height) according to \code{epsg_code} used. So, the numeric values can express different
#' types of measurement units, for example deegres or meters.
#' @return An object representing a study area containing a \code{sp} object.
#'
#' Occasional topological errors on polygons of the object are corrected. As a side effect holes inside
#' polygons are removed.
#'
#' If the \code{epsg_code} of the study area is different from the epsg_code passed to the function, the study
#' area is reproject.
#' @export
#' @examples
#' \dontrun{
#' SPDF <- readOGR(
#'    system.file("brasil_uf.gpkg", package="sdmTools"),
#'    layer = "brasil_uf",
#'    verbose = FALSE
#' )
#' new_sdm_area <- SPDF %>%
#'    sdm_area("Brasil", "EPSG:6933", c(50000, 50000)))
#'
#' class(new_sdm_area)
#'
#' plot(new_sdm_area)
#' }
#'
sdm_area <- function(an_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL, epsg_code = NULL, resolution = NULL){
  assert(
    check_list(var_names, types = "character", any.missing = F, all.missing = T, unique = T, null.ok = T),
    check_character(var_names, any.missing = F, all.missing = T, unique = T, null.ok = T)
  )
  sdm_area_name %>%
    assert_string(
      min.chars = 1,
      msg = "A modeling area (an_area) must have a name (sdm_area_name)."
    )
  epsg_code %>%
    assert_string(
      min.chars = 6,
      fixed = "EPSG:",
      msg = "A modeling area (an_area) must have a valid EPSG code (epsg_code) starting with 'EPSG:', for example EPSG:6933."
    )
  resolution %>%
    assert_number(
      lower = 0.0001,
      msg = "A modeling area (an_area) must have a resolution (resolution) expressed according to the EPSG code of the area."
    )
  dir_path %>%
    assert_string(
      min.chars = 1,
      msg = "A modeling area (an_area) must have a valid directory (dir_path) where data will be saved."
    )

  if(dir_path %>% dir_exists()){
    dir_path %>%
      dir_delete()
  }
  dir_path <- quiet(
    dir_path %>%
      dir_create()
  )
  dir_path %>%
    assert_directory_exists(
      msg = "A problem occurs on the directory creation (dir_path). A modeling area (an_area) must have a valid directory (dir_path) where data will be saved."
    )

  UseMethod("sdm_area", an_area)
}

#' @export
sdm_area.character <- function(an_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL, epsg_code = NULL, resolution = NULL){
  assert_file_exists(an_area)
  new_sdm_area <- an_area %>%
    readOGR(verbose = F) %>%
    .sp_sdm_area(
      sdm_area_name = sdm_area_name,
      epsg_code = epsg_code,
      resolution = resolution,
      var_names = var_names,
      dir_path = dir_path
    )

  return(new_sdm_area)
}

#' @export
sdm_area.Spatial <- function(an_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL, epsg_code = NULL, resolution = NULL){
  new_sdm_area <- an_area %>%
    .sp_sdm_area(
      sdm_area_name = sdm_area_name,
      epsg_code = epsg_code,
      resolution = resolution,
      var_names = var_names,
      dir_path = dir_path
    )

  return(new_sdm_area)
}

#' @noRd
#' @keywords internal
.sp_sdm_area <- function(an_area = NULL, var_names = NULL, sdm_area_name = NULL, dir_path = NULL, epsg_code = NULL, resolution = NULL){
  assert(
    check_class(an_area, "SpatialPolygons"),
    check_class(an_area, "SpatialLines"),
    .var.name = "an_area"
  )
  sdm_area_name %>%
    assert_string(
      min.chars = 1,
      msg = "A modeling area (an_area) must have a name (sdm_area_name)."
    )
  epsg_code <- epsg_code %>% toupper()
  epsg_code %>%
    assert_string(
      min.chars = 6,
      fixed = "EPSG:",
      msg = "A modeling area (an_area) must have a valid EPSG code (epsg_code) starting with 'EPSG:', for example EPSG:6933."
    )
  resolution %>%
    assert_number(
      lower = 0.0001,
      msg = "A modeling area (an_area) must have a resolution (resolution) expressed according to the EPSG code of the area."
    )
  assert_character(var_names, any.missing = F, all.missing = T, unique = T, min.len = 0, null.ok = T)
  dir_path %>%
    assert_directory_exists(
      msg = "A problem occurs on the directory creation (dir_path). A modeling area (an_area) must have a valid directory (dir_path) where data will be saved."
    )

  area_crs <- quiet(
    an_area %>%
      crs()
  )

  if(class(area_crs) == "try-error" || area_crs %>% is.na()){
    crs(an_area) <- crs("EPSG:4326")
  }

  crs_result <- quiet(
    epsg_code %>% crs()
  )
  crs_result %>%
    assert_class(
      classes = "CRS",
      msg = "A modeling area (an_area) must have a valid EPSG code (epsg_code) starting with 'EPSG:', for example EPSG:6933."
    )

  is_gridded <- quiet(
      an_area %>%
        .is_gridded()
  )

  if( is_gridded %>% is.na()){
    "An_area object has an invalid CRS!" %>%
      abort()
  }

  if (is_gridded){
    calculated_res <- an_area %>%
      .get_resolution()

    assert_number(calculated_res, lower = 0.0001)
    resolution %>%
      assert_number(
        lower = calculated_res,
        upper = calculated_res,
        msg = "The informed resolution (resolution) doesn't match to the resolution calculated from modeling area (an_area)."
      )
  }

  an_area <- an_area %>%
    spTransform(crs_result) %>%
    .repair_area()

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

  if (an_area %>% class() == "SpatialPolygons"){
    an_area <- an_area %>%
      as("SpatialPolygonsDataFrame")
  } else if (an_area %>% class() == "SpatialLines"){
    an_area <- an_area %>%
      as("SpatialLinesDataFrame")
  }
  if (var_found %>% length() > 0){
    an_area@data <- an_area@data %>%
      select(matches(var_found)) %>%
      set_names(var_found)
  } else {
    an_area@data <- an_area@data %>%
      select(-(ATTR_CONTROL_NAMES %>% as_vector() %>% any_of()))

    if (an_area %>% is("SpatialPolygons")){
      an_area@data <- 1:(an_area@polygons %>% length()) %>%
        as.data.frame() %>%
        rename(id=".")

    } else if (an_area %>% is("SpatialLines")){
      an_area@data <- 1:(an_area@lines %>% length()) %>%
        as.data.frame() %>%
        rename(id=".")
    }
  }

  sdm_area_tmp <- list(
    sdm_area_name = sdm_area_name,
    crs = crs_result,
    epsg_code = epsg_code,
    resolution = resolution,
    gridded = is_gridded,
    study_area = an_area,
    dir_path = dir_path,
    scenarios = NULL
  )

  an_area %>%
    save_gpkg(
      new_name = sdm_area_name,
      dir_path = dir_path
    )

  return(
    structure(
      sdm_area_tmp,
      class= "SDM_area"
    )
  )
}


#' @noRd
#' @keywords internal
.repair_area <- function(an_area = NULL){
  assert(
    check_class(an_area, "SpatialPolygons"),
    check_class(an_area, "SpatialLines")
  )

  crs_result <- quiet(
    an_area %>%
      crs()
  )

  crs_result %>%
    assert_class(
      classes = "CRS",
      msg = "A modeling area (an_area) must have a valid EPSG code (epsg_code) starting with 'EPSG:', for example EPSG:6933."
    )

  if (an_area %>% class() == "SpatialPolygons"){
    quiet(
      an_area <- an_area %>%
        gBuffer(byid = TRUE, width = 0)
    )
  }
  return(an_area)
}


#' @noRd
#' @keywords internal
.is_gridded <- function(an_area){
  an_area %>%
    assert_class("Spatial")
  if (!an_area %>% is("SpatialPolygonsDataFrame")) {
    return(F)
  }
  res_summ <- an_area %>%
    area() %>%
    summary()

  return(res_summ["Median"] == res_summ["Mean"] && (an_area@polygons %>% length() > 1))
}

#' @noRd
#' @keywords internal
.get_resolution <- function(an_area){
  an_area %>%
    assert_class("Spatial")
  if (an_area %>% .is_gridded()){
    coords <- an_area@polygons %>%
      extract2(1) %>%
      slot("Polygons") %>%
      extract2(1) %>%
      slot(("coords"))

    return(abs(coords[1,1] - coords[2,1]))
  }
  else {
    return(NULL)
  }
}
