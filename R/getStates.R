#' Get states
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve state list from the API.
#'
#' @details
#' To be added...
#'
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' listUnits,WorkspaceStudio-method
setGeneric("getStates", function(workspace, ...) {
  standardGeneric("getStates")
})

#' @describeIn listUnits List all units in a defined workspace
setMethod("getStates",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            headers = c(
              Authorization = workspace@login@token,
              "app-version" = workspace@login@version
            )

            domain <- workspace@login@domain
            ws_group_id <- workspace@group_id
            ws_group_label <- workspace@group_label

            request <- httr::GET(url = glue::glue(
              "{domain}/workspace-groups/{ws_group_id}"
            ),
            httr::add_headers(headers)
            )

            response <-
              httr::content(request) %>%
              purrr::pluck("settings", "states")

            if (!is.null(response)) {
              response <-
                response %>%
                purrr::list_transpose() %>%
                tibble::as_tibble()
            }


            return(response)
          })
