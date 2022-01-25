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

enum_as_vector<- function(a_enum){
  checkmate::assert_class(a_enum, "enumeration")
  a_enum %>% unlist(use.names = F)
}


ATTR_CONTROL_NAMES <- create_enum(dummy, cell_id, x_centroid, y_centroid)
RASTER_FORMATS_EXT <- create_enum(grd, asc, sdat, rst, nc, tif, envi, bil, img)
