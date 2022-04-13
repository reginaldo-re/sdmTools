flatten_scenario <- function(a_scenario) {
  assert(
    msg = "The variable names argument (var_names) should be a vector/list of non empty and non duplicated scenario.",
    a_scenario %>%
      check_list(min.len = 1, any.missing = F, all.missing = F),
    a_scenario %>%
      check_character(min.len = 1, any.missing = F, all.missing = F)
  )
  .flatten_scenario <- function(an_element) {
    return(
      an_element %>%
        discard(~ is.list(.)) %>%
        append(an_element %>%
                 keep(~ is.list(.)) %>%
                 map(~ .flatten_scenario(.)) %>%
                 flatten())
    )
  }
  if (!("content" %>% is_in(a_scenario %>% names()))){
    a_scenario = list(content = a_scenario)
  }

  return(
    a_scenario$content %>%
      .flatten_scenario()
  )
}
