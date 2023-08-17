#' List booklet files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve booklet list from the API.
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
#' listBooklets,Workspace-method
setGeneric("listBooklets", function(workspace) {
  standardGeneric("listBooklets")
})

#' @describeIn listBooklets List all booklets in a defined workspace
setMethod("listBooklets",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_booklets <- listFiles(workspace, type = "Booklet")

            ws_booklets %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
