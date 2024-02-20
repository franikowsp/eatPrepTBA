#' Get multiple testtakers files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve testtakers files from the API.
#' @param id Character vector  (optional). Names of the testtakers files to be retrieved. If no id is set, all testtakers files in the workspace are retrieved.
#'
#' @description
#' This function returns information for multiple testtakers files by repeatedly calling [getTesttaker()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getTesttakers,Workspace-method
setGeneric("getTesttakers", function(workspace, ...) {
  standardGeneric("getTesttakers")
})

#' @describeIn getTesttakers Get multiple testtakers files in a defined workspace
setMethod("getTesttakers",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, id = NULL) {
            getFiles(
              workspace = workspace,
              id = id,
              type = "testtakers",
              listFun = listTesttakers,
              getFun = getTesttaker
            )
          })

