#' Change settings of multiple units
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param id Character. Names of the units to be changed
#'
#' @description
#' This function only has side effects on the IQB Studio.
#'
#' @return NULL
#' @examples
#' @aliases
#' changeUnitsSettings,WorkspaceStudio-method
#' @export
setGeneric("changeUnitsSettings", function(workspace, id, ...) {
  standardGeneric("changeUnitsSettings")
})


#' @describeIn changeUnitSettings Get unit information and coding scheme in a defined workspace
setMethod("changeUnitsSettings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, id, ...) {
            id %>%
              purrr::walk(
                function(id) {
                  changeUnitSettings(
                    workspace,
                    id,
                    ...
                  )

                }, .progress = TRUE
              )
          })
