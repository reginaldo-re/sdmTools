.quiet <- function (expr) {
  withr::with_options(
    list(warn=-1),
    {
      withCallingHandlers(
        force(expr),
        warning = function(w) invokeRestart("muffleWarning"),
        message = function(e) invokeRestart("muffleMessage")
      )
    }
  )
}


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
