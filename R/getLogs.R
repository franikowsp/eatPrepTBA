#' Get logs
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved  or all groups if not specified.
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
setGeneric("getLogs", function(workspace, groups = NULL) {
  standardGeneric("getLogs")
})

#' @describeIn getResponses Get responses of a defined workspace
setMethod("getLogs",
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

            params <- list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/report/log"
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
                  value = purrr::map(value, \(x) tibble::as_tibble(x))
                ) %>%
                # Entpacken
                tidyr::unnest(value)
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
