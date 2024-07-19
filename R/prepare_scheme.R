prepare_scheme <- function(unit, resp_scheme) {
  coding_scheme <-
    resp_scheme$scheme %>%
    jsonlite::parse_json()

  unit <-
    unit %>%
    dplyr::mutate(coding_scheme = list(coding_scheme))
}
