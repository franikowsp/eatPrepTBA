#' List groups
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve group list from the API.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' @aliases
#' listUnits,WorkspaceStudio-method
setGeneric("listGroups", function(workspace, ...) {
  standardGeneric("listGroups")
})

#' @describeIn listGroups List all groups in a defined workspace
setMethod("listGroups",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            headers = c(
              Authorization = workspace@login@token,
              "app-version" = workspace@login@version
            )

            domain <- workspace@login@domain
            ws_id <- workspace@id
            ws_label <- workspace@label

            request <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/groups"
            ),
            httr::add_headers(headers)
            )

            response <-
              httr::content(request) %>%
              purrr::list_simplify()

            return(response)
          })
