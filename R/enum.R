create_enum <- function(...) {
  allowed_values <- match.call(expand.dots = TRUE)[-1L] %>%
    sapply(deparse)

  stopifnot(identical(unique(allowed_values), allowed_values))

  allowed_values  <- allowed_values %>%
    make.names(unique = TRUE)

  new_enum <- allowed_values %>%
    setNames(allowed_values) %>%
    as.list()

  class(new_enum) <- "enumeration" %>%
    append(class(new_enum))

  new_enum %>%
    return()
}

as_vector <- function(an_enum = NULL){
  UseMethod("as_vector", an_enum)
}

as_vector.enumeration <- function(an_enum = NULL){
  an_enum %>%
    unlist(use.names = F)
}

contains <- function(an_enum = NULL, an_item = NULL){
  UseMethod("contains", an_enum)
}

contains.enumeration <- function(an_enum = NULL, an_item = NULL){
  an_item %>%
    tolower() %>%
    magrittr::is_in(an_enum %>% tolower()) %>%
    return()
}

ATTR_CONTROL_NAMES <- create_enum(dummy, cell_id, x_centroid, y_centroid)
RAST_FORMATS_EXT <- create_enum(grd, asc, sdat, rst, nc, tif, envi, bil, img)
VECT_FORMATS_EXT <- create_enum(svg, gpkg)
