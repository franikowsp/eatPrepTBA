#' Reads and prepares testtakers file
#'
#' @param file Character. Path to the csv file from the IQB Testcenter to be read.
#'
#' @description
#' This function only returns the testtakers information for a downloaded testtakers file.
#'
#' @return A tibble.
#'
#' @export
read_responses <- function(file) {
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
    dplyr::rename(any_of(c(
      group_id = "groupname",
      login_name = "loginname",
      code = "code",
      booklet_id = "bookletname",
      unit_key = "unitname",
      player = "PLAYER",
      presentation_progress = "PRESENTATION_PROGRESS",
      response_progress = "RESPONSE_PROGRESS",
      page_no = "CURRENT_PAGE_NR",
      page_id = "CURRENT_PAGE_ID",
      page_count = "PAGE_COUNT",
      variable_id = "id",
      value = "value",
      status = "status"
    )))
}
