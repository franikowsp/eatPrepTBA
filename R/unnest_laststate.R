unnest_laststate <- function(json) {
  if (all(is.na(json))) {
    return(tibble::tibble(PLAYER = NA_character_))
  }

  if (length(json) == 1) {
    laststate_tbl <-
      json %>%
      jsonlite::parse_json(simplifyVector = TRUE) %>%
      tibble::as_tibble()
  } else {
    json_filter <- json[!is.na(json)]

    laststate_tbl <-
      json_filter %>%
      purrr::map(
        function(x) {
          x %>%
            jsonlite::parse_json(simplifyVector = TRUE) %>%
            tibble::as_tibble()
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::distinct()
  }

  if (nrow(laststate_tbl) == 1) {
    return(laststate_tbl)
  } else {
    laststate_tbl %>%
      tidyr::fill(dplyr::everything(), .direction = "downup") %>%
      dplyr::filter(RESPONSE_PROGRESS == "complete")
  }
}
