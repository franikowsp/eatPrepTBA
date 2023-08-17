#' Get multiple units with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve multiple unit information and resources (coding schemes) from the API.
#' @param id Character vector (optional). Names of the units to be retrieved. If no id is set, all units files and their resources in the workspace are retrieved.
#'
#' @description
#' This function returns the unit information for multiple units by repeatedly calling [getUnit()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getUnits,Workspace-method
setGeneric("getUnits", function(workspace, ...) {
  standardGeneric("getUnits")
})

#' @describeIn getUnits Get multiple unit information and coding schemes in a defined workspace
setMethod("getUnits",
          signature = signature(workspace = "Workspace"),
          function(workspace, id = NULL) {
            getFiles(
              workspace = workspace,
              id = id,
              type = "unit",
              listFun = listUnits,
              getFun = getUnit
            )
          })
