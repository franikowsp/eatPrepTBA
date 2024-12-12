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

  # TODO: This is only necessary due to bugs in older TC versions (2024)
  if (nrow(laststate_tbl) == 1) {
    return(laststate_tbl)
  } else {
    # This (hopefully) addresses the problem that there are multiple laststates around
    laststate_tbl <-
      laststate_tbl %>%
      tidyr::fill(dplyr::everything(), .direction = "downup") %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c("PLAYER", "RESPONSE_PROGRESS", "PRESENTATION_PROGRESS")),
          function(x) {
            update_laststate(information = dplyr::cur_column(), current_states = x)
          }),
      ) %>%
      dplyr::distinct()

    if (nrow(laststate_tbl) == 1) {
      return(laststate_tbl)
    } else {
      # If it cannot be harmonized, this error will be thrown
      tibble::tibble(laststate_error = TRUE)
    }
  }
}
