#' List testtakers files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve testtakers list from the API.
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
#' listTesttakers,Workspace-method
setGeneric("listTesttakers", function(workspace) {
  standardGeneric("listTesttakers")
})

#' @describeIn listTesttakers List all testtakers in a defined workspace
setMethod("listTesttakers",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_testtakers <- listFiles(workspace, type = "Testtakers")

            ws_testtakers %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
