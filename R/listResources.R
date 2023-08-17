#' List unit resource files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit resource list (coding scheme and aspect data) from the API.
#'
#' @description
#' This function serves as a wrapper for [getFiles()].
#'
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' listResources,Workspace-method
setGeneric("listResources", function(workspace) {
  standardGeneric("listResources")
})

#' @describeIn listResources List all unit resources in a defined workspace
setMethod("listResources",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_resources <- listFiles(workspace, type = "Resource")

            ws_resources %>%
              dplyr::filter(stringr::str_detect(name, "\\.voud") |
                              stringr::str_detect(name, "\\.vocs")) %>%
              dplyr::pull("name")
          })


