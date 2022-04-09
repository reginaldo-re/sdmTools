.check <- function(fun = NULL, ..., msg = NULL){
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
  result <- .check(fun = fun, ..., msg = msg)
  if (result %>% is.logical()){
    return(result)
  } else {
    msg %>%
      abort()
  }
}

nested_check <- function(..., combine = "or"){
  all_calls <- rlang::enquos(...)

  check_calls <- all_calls %>%
    discard(~ rlang::call_name(.) == "nested_check")

  results_check_calls <- check_calls %>%
    map(~ rlang::eval_tidy(.))

  if (combine == "or"){
    if ((results_check_calls == T) %>% any()){
      return(T)
    } else {
      results_check_calls <- results_check_calls %>%
        discard(~ . == T)

      nested_check_calls <- all_calls %>%
        keep(~ rlang::call_name(.) == "nested_check")

      results_nested_check_calls <-  nested_check_calls %>%
        map(~ rlang::eval_tidy(.))

      if ((results_nested_check_calls ==T) %>% any()){
        return(T)
      } else {
        return(
          c(
            results_check_calls,
            results_nested_check_calls
          )
        )
      }
    }
  } else{
    results_check_calls <- results_check_calls %>%
      discard(~ . == T)

    nested_check_calls <- all_calls %>%
      keep(~ rlang::call_name(.) == "nested_check")

    results_nested_check_calls <- nested_check_calls %>%
      map(~ rlang::eval_tidy(.)) %>%
      flatten()

    if (results_nested_check_calls %>% length() > 0){
      results_nested_check_calls <- results_nested_check_calls %>%
        discard(~ . == T)
    }

    results_all_calls <- c(
      results_check_calls,
      results_nested_check_calls
    )

    if (results_all_calls %>% length() > 0){
      return(results_all_calls)
    } else {
      return(T)
    }
  }
}

assert <- function(..., msg = NULL, combine = "or"){
  invalid_calls <- rlang::exprs(...) %>%
    map(~ .x %>% rlang::expr_name() %>% str_starts("assert")) %>%
    unlist() %>%
    any()
  if (invalid_calls){
    "There exists at least one invalid call to assert function or assert_that function. You must use only nested_check calls." %>%
      rlang::abort()
  }

  results_check_calls <- list(...) %>%
    unlist() %>%
    as.character()

  if (combine == "or"){
    if ((results_check_calls == T) %>% any()){
      return(T)
    } else {
      c(msg, results_check_calls) %>%
        abort()
    }
  } else {
    if ((results_check_calls == T) %>% all()){
      return(T)
    } else {
      c(msg, results_check_calls) %>%
        abort()
    }
  }
}

check_string <- function(..., msg = NULL){
  return(.check(fun = "string", ..., msg = msg))
}

assert_string <- function(..., msg = NULL){
  return(.result_or_abort(fun = "string", ..., msg = msg))
}

check_number <- function(..., msg = NULL){
  return(.check(fun = "number", ..., msg = msg))
}

assert_number <- function(..., msg = NULL){
  return(.result_or_abort(fun = "number", ..., msg = msg))
}

check_directory_exists <- function(..., msg = NULL){
  return(.check(fun = "directory_exists", ..., msg = msg))
}

assert_directory_exists <- function(..., msg = NULL){
  return(.result_or_abort(fun = "directory_exists", ..., msg = msg))
}

check_class <- function(..., msg = NULL){
  return(.check(fun = "class", ..., msg = msg))
}

assert_class <- function(..., msg = NULL){
  return(.result_or_abort(fun = "class", ..., msg = msg))
}

check_subset <- function(..., msg = NULL){
  return(.check(fun = "subset", ..., msg = msg))
}

assert_subset <- function(..., msg = NULL){
  return(.result_or_abort(fun = "subset", ..., msg = msg))
}

check_int <- function(..., msg = NULL){
  return(.check(fun = "int", ..., msg = msg))
}

assert_int <- function(..., msg = NULL){
  return(.result_or_abort(fun = "int", ..., msg = msg))
}

check_true <- function(..., msg = NULL){
  return(.check(fun = "true", ..., msg = msg))
}

assert_true <- function(..., msg = NULL){
  return(.result_or_abort(fun = "true", ..., msg = msg))
}

check_false <- function(..., msg = NULL){
  return(.check(fun = "false", ..., msg = msg))
}

assert_false <- function(..., msg = NULL){
  return(.result_or_abort(fun = "false", ..., msg = msg))
}

check_character <- function(..., msg = NULL){
  return(.check(fun = "character", ..., msg = msg))
}

assert_character <- function(..., msg = NULL){
  return(.result_or_abort(fun = "character", ..., msg = msg))
}

