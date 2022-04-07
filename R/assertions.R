check_ <- function(fun = NULL, ..., msg = NULL){
  checkmate::assert(
    checkmate::check_character(msg, any.missing = F, all.missing = F, min.len = 1),
    checkmate::check_null(msg)
  )
  result <- "check_" %>%
    paste0(fun) %>%
    rlang::call2(..., .ns = "checkmate") %>%
    rlang::eval_tidy()

  if (result %>% is.logical()){
    return(result)
  } else {
    result <- ifelse(
      msg %>% is.null(),
      result,
      msg
    )
  }
  return(result)
}

.result_or_abort <- function(fun = NULL, ..., msg = NULL){
  result <- check_(fun = fun, ..., msg = msg)
  if (result %>% is.logical()){
    return(result)
  } else {
    msg %>%
      abort()
  }
}

check_string <- function(..., msg = NULL){
  return(check_(fun = "string", ..., msg = msg))
}

assert_string <- function(..., msg = NULL){
  return(.result_or_abort(fun = "string", ..., msg = msg))
}

check_number <- function(..., msg = NULL){
  return(check_(fun = "number", ..., msg = msg))
}

assert_number <- function(..., msg = NULL){
  return(.result_or_abort(fun = "number", ..., msg = msg))
}

check_directory_exists <- function(..., msg = NULL){
  return(check_(fun = "directory_exists", ..., msg = msg))
}

assert_directory_exists <- function(..., msg = NULL){
  return(.result_or_abort(fun = "directory_exists", ..., msg = msg))
}

check_class <- function(..., msg = NULL){
  return(check_(fun = "class", ..., msg = msg))
}

assert_class <- function(..., msg = NULL){
  return(.result_or_abort(fun = "class", ..., msg = msg))
}

check_subset <- function(..., msg = NULL){
  return(check_(fun = "subset", ..., msg = msg))
}

assert_subset <- function(..., msg = NULL){
  return(.result_or_abort(fun = "subset", ..., msg = msg))
}

check_int <- function(..., msg = NULL){
  return(check_(fun = "int", ..., msg = msg))
}

assert_int <- function(..., msg = NULL){
  return(.result_or_abort(fun = "int", ..., msg = msg))
}

check_true <- function(..., msg = NULL){
  return(check_(fun = "true", ..., msg = msg))
}

assert_true <- function(..., msg = NULL){
  return(.result_or_abort(fun = "true", ..., msg = msg))
}

check_character <- function(..., msg = NULL){
  return(check_(fun = "character", ..., msg = msg))
}

assert_character <- function(..., msg = NULL){
  return(.result_or_abort(fun = "character", ..., msg = msg))
}

