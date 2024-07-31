#' Get responses
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#' @param prepare Logical. Should the data be prepared, i.e., should the JSON objects in the laststate and the responses be unpacked? Defaults to `TRUE`.
#' @param debug Logical. Should a debugging applied to the Testcenter data, i.e., deleting empty rows or state variables. Defaults to `FALSE`.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_responses,WorkspaceTestcenter-method
setGeneric("get_responses", function(workspace,
                                     groups = NULL,
                                     prepare = TRUE,
                                     debug = FALSE) {
  cli_setting()

  standardGeneric("get_responses")
})

#' @describeIn get_responses Get responses of a given Testcenter workspace
setMethod("get_responses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL,
                   prepare = TRUE,
                   debug = FALSE) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "report", "response"),
                       query = list(dataIds = group)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            # resp <-
            #   run_safe(run_req,
            #            error_message = "Responses could not be retrieved.")

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading responses")

            if (!is.null(resp)) {
              responses <-
                resp %>%
                purrr::flatten() %>%
                # Rectangularize (zu tibble)
                tibble::enframe(name = NULL) %>%
                # Schleife zum Spreaden der Einträge (Auslesen in tibble)
                dplyr::mutate(
                  # Ladebalken?
                  value = purrr::map(value, function(x) {
                    x %>%
                      purrr::discard(is.null) %>%
                      tibble::as_tibble()
                  })
                ) %>%
                # Entpacken
                tidyr::unnest(value)

                if (prepare) {
                  responses <-
                    responses %>%
                    # Schleife zum Spreaden der
                    # Response- und LastState-Einträge (Auslesen in tibble)
                    dplyr::mutate(
                      responses = purrr::map(responses, function(x) {
                        responses <- x$content %>%
                          jsonlite::parse_json(simplifyVector = TRUE) %>%
                          tibble::as_tibble()

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
                    # Entpacken
                    tidyr::unnest(c(
                      responses,
                      laststate
                    )) %>%
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
                    ))
                    )
                }

                responses
              # if (debug) {
              #   responses <-
              #     responses %>%
              #     dplyr::group_by(
              #       groupname, loginname, code, bookletname, unitname
              #     ) %>%
              #     tidyr::nest(
              #       data = c(responses, laststate)
              #     ) %>%
              #     dplyr::mutate(
              #       n = purrr::map_int(data, nrow),
              #       data = purrr::map2(n, data, function(n, x) {
              #         new_x <- x
              #         if (n != 1) {
              #           new_x <-
              #             x %>%
              #             dplyr::filter(responses != "[]")
              #
              #           if (nrow(new_x) != 1) {
              #             new_x <-
              #               new_x %>%
              #               dplyr::filter(!is.na(laststate))
              #           }
              #         }
              #
              #         new_x
              #       }),
              #       n = purrr::map_int(data, nrow),
              #     ) %>%
              #     # dplyr::filter(n != 1)
              #     dplyr::select(-n) %>%
              #     tidyr::unnest(data) %>%
              #     dplyr::mutate(
              #       laststate = ifelse(is.na(laststate), "{}", laststate)
              #     )
              # }
            } else {
              tibble::tibble()
            }
          })
