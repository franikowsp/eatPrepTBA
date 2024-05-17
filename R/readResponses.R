#' Reads and prepares testtakers file
#'
#' @param file Chracter. Path to the csv file to be read.
#'
#' @description
#' This function only returns the testtakers information for a downloaded testtakers file.
#'
#' @return A tibble.
#'
#'
#' @examples
#' @export
readResponses <- function(file) {
  responses_raw <- readr::read_delim(file, delim = ";")

  responses_raw %>%
    dplyr::relocate(laststate, .before = responses) %>%
    dplyr::mutate(
      responses = purrr::map(responses, function(x) {
        content <-
          x %>%
          jsonlite::parse_json() %>%
          purrr::pluck(1, "content")

        if (!is.null(content)) {
          responses <- content %>%
            jsonlite::parse_json(simplifyVector = TRUE) %>%
            tibble::as_tibble()
        } else {
          responses <- tibble::tibble(id = NA)
        }

        if (tibble::has_name(responses, "value")) {
          responses %>%
            dplyr::mutate(
              value = purrr::map(value, as.list)
            )
        } else {
          responses
        }
      }),
      laststate = purrr::map(laststate, function(x) {
        if (!is.na(x)) {
          x %>%
            jsonlite::parse_json(simplifyVector = TRUE) %>%
            tibble::as_tibble()
        } else {
          tibble::tibble(PLAYER = NA_character_)
        }
      })
    ) %>%
    tidyr::unnest(
      c(laststate, responses)
    ) %>%
    dplyr::rename(
      variable_id = id
    )
}
