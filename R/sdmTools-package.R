#' @keywords internal
#' @import tibble
#' @import ggplot2
#' @import purrr
#' @import dplyr
#' @import fs
#' @import stringr
#' @import checkmate
#' @rawNamespace import(magrittr, except = c("set_names"))
#' @rawNamespace import(raster, except = c("select", "union", "extract", "intersect"))
#' @rawNamespace import(rgeos, except = c("union", "setdiff", "intersect"))
#' @rawNamespace import(terra, except = c("union", "extract", "inset", "src", "intersect", "arrow", "describe"))
#' @importFrom methods as
#' @importFrom methods is
#' @importFrom methods slot
#' @importFrom foreach %dopar%
#' @importFrom stats setNames
#' @importFrom lifecycle deprecated
#' @importFrom rlang ":="
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom gdalUtils gdalwarp
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom sp spChFIDs
#' @importFrom sp spTransform
#' @importFrom rgdal readOGR
#' @importFrom rgdal writeOGR
#' @importFrom tidyselect matches
#' @importFrom tidyselect any_of
#' @importFrom broom tidy
#' @importFrom tibble rowid_to_column
#' @importFrom waldo compare
#' @importFrom withr with_options
#' @importFrom withr with_dir
#' @importFrom withr with_tempdir
#' @importFrom snakecase to_snake_case
#' @importFrom stringi stri_rand_strings
"_PACKAGE"

## usethis namespace: start
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

