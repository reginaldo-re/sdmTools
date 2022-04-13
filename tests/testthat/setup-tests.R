# library(sp)
# library(raster)
# library(rgeos)
# library(rgdal)
# library(gdalUtils)
# library(magrittr)
# library(dplyr)
# library(purrr)
# library(tibble)
# library(checkmate)
#
#
# main <-  cbind(
#   c(0, 0, 1, 1),
#   c(0, 1, 1, 0)
# )
# secondary <- cbind(
#   c(1, 1.3, 1.3, 1),
#   c(1, 1.0, 0.7, 0.7)
# )
# hole <- main/3 + 1/3
# island = cbind(
#   c(1.05, 1.05, 1.55, 1.55),
#   c(0, .5, .5, 0)
# )
#
# P <- Polygons(
#   ID = 1,
#   list(
#     Polygon(main),
#     Polygon(hole, hole = TRUE),
#     Polygon(island),
#     Polygon(secondary)
#   )
# )
#
# SP <- SpatialPolygons(
#     list(
#       Polygons(
#         ID = 1,
#         list(
#           Polygon(main),
#           Polygon(hole, hole = TRUE)
#         )
#       ),
#       Polygons(
#         ID = 2,
#         list(
#           Polygon(island)
#         )
#       ),
#       Polygons(
#         ID = 3,
#         list(
#           Polygon(secondary)
#         )
#       )
#   )
# )
# crs(SP) <- CRS("EPSG:6933")
#
# SL = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
# crs(SL) <- CRS("EPSG:6933")
#
# SPDF <- readOGR(system.file("vect_files/brasil_uf.gpkg", package="sdmTools"), layer = "brasil_uf", verbose = F)
# SLDF <- readOGR(system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools"), layer = "hydro_uper_prpy", verbose = F)
#
# a_sdm_area <- SPDF %>%
#   sdm_area(
#     sdm_area_name = "Test area",
#     epsg_code = "EPSG:6933",
#     resolution = 50000,
#     dir_path = "/tmp/setup_test"
#   )
#
#
# a_sdm_area_gridded_area <- a_sdm_area %>%
#    make_grid()
