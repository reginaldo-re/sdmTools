sdm_area_composite <- function(a_path = NULL) {
  UseMethod("sdm_area_composite", a_path)
}

character.sdm_area_composite <- function(a_path = NULL) {
  checkmate::assert_string(a_path)

}
