#' Get responses
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getResponses,Workspace-method
setGeneric("getResponses", function(workspace, groups) {
  standardGeneric("getResponses")
})

#' @describeIn getResponses Get responses of a defined workspace
setMethod("getResponses",
          signature = signature(workspace = "Workspace"),
          function(workspace, groups) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            params = list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{login@domain}/workspace/{ws_id}/report/response"
            ),
            httr::add_headers(.headers=headers),
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
                  value = purrr::map(value, \(x) tibble::as_tibble(x))
                ) %>%
                # Entpacken
                tidyr::unnest(value) %>%
                # Schleife zum Spreaden der Response- und LastState-Einträge (Auslesen in tibble)
                dplyr::mutate(
                  responses = purrr::map(responses, \(x) x$content %>%
                                           jsonlite::parse_json(simplifyVector = TRUE) %>%
                                           tibble::as_tibble()),
                  laststate = purrr::map(laststate, \(x) x %>%
                                           jsonlite::parse_json(simplifyVector = TRUE) %>%
                                           tibble::as_tibble()),
                ) %>%
                # Entpacken
                tidyr::unnest(c(
                  responses,
                  laststate
                ))
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
