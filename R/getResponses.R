#' Get responses
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getResponses,WorkspaceTestcenter-method
setGeneric("getResponses", function(workspace, groups = NULL) {
  standardGeneric("getResponses")
})

#' @describeIn getResponses Get responses of a defined workspace
setMethod("getResponses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            if (is.null(groups)) {
              testtakers <- getTesttakers(workspace)
              groups <- unique(testtakers$groupname)
            }

            params = list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/report/response"
            ),
            httr::add_headers(.headers = headers),
            query = params
            )

            if (request_json$status_code == 200) {
              response_json <-
                httr::content(request_json,
                              type = "application/json",
                              encoding = "UTF-8") %>%
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
                tidyr::unnest(value) %>%
                # Schleife zum Spreaden der Response- und LastState-Einträge (Auslesen in tibble)
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
                dplyr::rename(
                  variable_id = id
                )
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
