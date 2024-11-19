#' #' Prepares responses
#' #'
#' #' @param files Character. Vector of paths to the csv files from the IQB Testcenter to be read.
#' #' @param prepare Logical. Should the data be prepared, i.e., should the JSON objects in the laststate and the responses be unpacked? Defaults to `TRUE`.
#'
#' #'
#' #' @description
#' #' This function only returns the testtakers information for a downloaded testtakers file.
#' #'
#' #' @return A tibble.
#' #'
#' #' @export
#' prepare_responses <- function(files, prepare = TRUE) {
#'   if (length(files) == 1) {
#'     responses_raw <-
#'       readr::read_delim(file, delim = ";")
#'   } else {
#'     responses_raw <-
#'       tibble::tibble(
#'         file = files
#'       ) %>%
#'       dplyr::mutate(
#'         data = purrr::map(readr::read_delim(file, delim = ";")
#'       )
#'   }
#'
#'   responses_raw <-
#'     readr::read_delim(file, delim = ";") %>%
#'     dplyr::select(
#'       dplyr::any_of(c(
#'         group_id = "groupname",
#'         login_name = "loginname",
#'         login_code = "code",
#'         booklet_id = "bookletname",
#'         unit_key = "originalUnitId",
#'         unit_alias = "unitname",
#'         responses_nest = "responses",
#'         laststate_nest = "laststate"
#'       ))
#'     )
#'
#'   if (prepare) {
#'     responses_raw %>%
#'       dplyr::relocate(laststate, .before = responses) %>%
#'       dplyr::mutate(
#'         responses = purrr::map(responses, function(x) {
#'           content <-
#'             x %>%
#'             jsonlite::parse_json() %>%
#'             purrr::pluck(1, "content")
#'
#'           if (!is.null(content)) {
#'             responses <- content %>%
#'               jsonlite::parse_json(simplifyVector = TRUE) %>%
#'               tibble::as_tibble()
#'           } else {
#'             responses <- tibble::tibble(id = NA)
#'           }
#'
#'           if (tibble::has_name(responses, "value")) {
#'             responses %>%
#'               dplyr::mutate(
#'                 value = purrr::map(value, as.list)
#'               )
#'           } else {
#'             responses
#'           }
#'         }),
#'         laststate = purrr::map(laststate, function(x) {
#'           if (!is.na(x)) {
#'             x %>%
#'               jsonlite::parse_json(simplifyVector = TRUE) %>%
#'               tibble::as_tibble()
#'           } else {
#'             tibble::tibble(PLAYER = NA_character_)
#'           }
#'         })
#'       ) %>%
#'       tidyr::unnest(
#'         c(laststate, responses)
#'       ) %>%
#'       dplyr::rename(any_of(c(
#'         group_id = "groupname",
#'         login_name = "loginname",
#'         code = "code",
#'         booklet_id = "bookletname",
#'         unit_key = "unitname",
#'         player = "PLAYER",
#'         presentation_progress = "PRESENTATION_PROGRESS",
#'         response_progress = "RESPONSE_PROGRESS",
#'         page_no = "CURRENT_PAGE_NR",
#'         page_id = "CURRENT_PAGE_ID",
#'         page_count = "PAGE_COUNT",
#'         variable_id = "id",
#'         value = "value",
#'         status = "status"
#'       )))
#'   } else {
#'     responses_raw
#'   }
#'
#' }
# from get_responses()
#
#             #     if (prepare) {
#       responses <-
#         responses %>%
#         # Schleife zum Spreaden der
#         # Response- und LastState-Einträge (Auslesen in tibble)
#         dplyr::mutate(
#           responses = purrr::map(responses, function(x) {
#             responses <- x$content %>%
#               jsonlite::parse_json(simplifyVector = TRUE) %>%
#               tibble::as_tibble()
#
#             if (tibble::has_name(responses, "value")) {
#               responses %>%
#                 dplyr::mutate(
#                   value = purrr::map(value, as.list)
#                 )
#             } else {
#               responses
#             }
#
#           }),
#           laststate = purrr::map(laststate, function(x) {
#             if (!is.na(x)) {
#               x %>%
#                 jsonlite::parse_json(simplifyVector = TRUE) %>%
#                 tibble::as_tibble()
#             } else {
#               tibble::tibble(PLAYER = NA_character_)
#             }
#           })
#         ) %>%
#         # Entpacken
#         tidyr::unnest(c(
#           responses,
#           laststate
#         )) %>%
#         dplyr::rename(any_of(c(
#           group_id = "groupname",
#           login_name = "loginname",
#           code = "code",
#           booklet_id = "bookletname",
#           unit_key = "unitname",
#           player = "PLAYER",
#           presentation_progress = "PRESENTATION_PROGRESS",
#           response_progress = "RESPONSE_PROGRESS",
#           page_no = "CURRENT_PAGE_NR",
#           page_id = "CURRENT_PAGE_ID",
#           page_count = "PAGE_COUNT",
#           variable_id = "id",
#           value = "value",
#           status = "status"
#         ))
#         )
#     }
#
