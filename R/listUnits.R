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
setGeneric("listUnits", function(workspace) {
  standardGeneric("listUnits")
})

#' @describeIn listUnits List all units in a defined workspace
setMethod("listUnits",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_units <- listFiles(workspace, type = "Unit")

            ws_units %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
