ATTR_CONTROL_NAMES <- NULL
RAST_FORMATS_EXT <- NULL
VECT_FORMATS_EXT <- NULL

.onLoad <- function(libname, pkgname) {
  ATTR_CONTROL_NAMES <<- create_enum(dummy, cell_id, x_centroid, y_centroid)
  RAST_FORMATS_EXT <<- create_enum(grd, asc, sdat, rst, nc, tif, envi, bil, img)
  VECT_FORMATS_EXT <<- create_enum(svg, gpkg)
}


