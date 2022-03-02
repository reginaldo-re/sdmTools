flatten_scenario <- function(a_scenario) {
  checkmate::assert(
    checkmate::check_list(a_scenario, min.len = 1, any.missing = F, all.missing = F),
    checkmate::check_character(a_scenario, min.len = 1, any.missing = F, all.missing = F)
  )
  .flatten_scenario <- function(an_element) {
    return(
      an_element %>%
        purrr::discard(~ is.list(.)) %>%
        append(an_element %>%
                 purrr::keep(~ is.list(.)) %>%
                 map(~ .flatten_scenario(.)) %>%
                 flatten())
    )
  }
  if (!("content" %>% magrittr::is_in(a_scenario %>% names()))){
    a_scenario = list(content = a_scenario)
  }

  return(
    a_scenario$content %>%
      .flatten_scenario()
  )
}
