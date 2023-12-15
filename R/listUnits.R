#' List unit files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit list from the API.
#'
#' @details
#' This function serves as a wrapper for [getFiles()].
#'
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' listUnits,Workspace-method
setGeneric("listUnits", function(workspace, ...) {
  standardGeneric("listUnits")
})

#' @describeIn listUnits List all units in a defined workspace
setMethod("listUnits",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            ws_units <- listFiles(workspace, type = "Unit")

            ws_units %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })

#' @describeIn listUnits List all units in a defined workspace
setMethod("listUnits",
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
              "{domain}/workspace/{ws_id}/units"
            ),
            httr::add_headers(headers)
            )

            response <-
              httr::content(request) %>%
              purrr::map(function(x) {
                out <- x[["id"]]
                names(out) <- x[["key"]]

                return(out)
              }) %>%
              purrr::reduce(append)

            return(response)
          })
