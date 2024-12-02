unnest_laststate <- function(json) {
  if (!is.na(json)) {
    json %>%
      jsonlite::parse_json(simplifyVector = TRUE) %>%
      tibble::as_tibble()
  } else {
    tibble::tibble(PLAYER = NA_character_)
  }
}
