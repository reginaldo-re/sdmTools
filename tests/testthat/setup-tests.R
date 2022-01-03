library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(magrittr)
library(dplyr)
library(purrr)
library(tibble)
library(checkmate)


main <-  cbind(
  c(0, 0, 1, 1),
  c(0, 1, 1, 0)
)
secondary <- cbind(
  c(1, 1.3, 1.3, 1),
  c(1, 1.0, 0.7, 0.7)
)
hole <- main/3 + 1/3
island = cbind(
  c(1.05, 1.05, 1.55, 1.55),
  c(0, .5, .5, 0)
)

P <- Polygons(
  ID = 1,
  list(
    Polygon(main),
    Polygon(hole, hole = TRUE),
    Polygon(island),
    Polygon(secondary)
  )
)

SP = SpatialPolygons(list(P))
crs(SP) <- CRS("EPSG:6933")

SPDF <- rgdal::readOGR(system.file("vect_files/brasil_uf.gpkg", package="sdmTools"), layer = "brasil_uf", verbose = F)
SLDF <- rgdal::readOGR(system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools"), layer = "hydro_uper_prpy", verbose = F)

a_sdm_area <- SPDF %>%
  sdm_area("Test area", "EPSG:6933", c(50000, 50000))


a_sdm_area_gridded_area <- a_sdm_area %>%
  make_grid()
