#' Get results
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve results information.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getResults,WorkspaceTestcenter-method
setGeneric("getResults", function(workspace) {
  standardGeneric("getResults")
})

#' @describeIn getResults Get responses of a defined workspace
setMethod("getResults",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/results"
            ),
            httr::add_headers(.headers=headers)
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
                tidyr::unnest(value)
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
