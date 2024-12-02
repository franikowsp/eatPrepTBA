#' Get multiple units with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_ids Integer (optional). Vector of the IDs of the unit to be retrieved. If no ID is given, all units files and their resources in the workspace are retrieved.
#' @param unit_definition Logical. Should the unit definition be added? Defaults to `FALSE`.
#' @param metadata Logical. Should the metadata be added? Defaults to `TRUE`.
#' @param coding_scheme Logical. Should the coding scheme be added? Defaults to `FALSE`.
#' @param verbose Logical. Should the function return additional messages. Defaults to `FALSE`.
#'
#' @description
#' This function returns the unit information for multiple units by repeatedly calling [get_unit()].
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_units,WorkspaceTestcenter-method,WorkspaceStudio-method
setGeneric("get_units", function(workspace,
                                 unit_ids = NULL,
                                 metadata = TRUE,
                                 unit_definition = FALSE,
                                 coding_scheme = FALSE,
                                 verbose = FALSE) {
  standardGeneric("get_units")
})

#' @describeIn get_units Get multiple unit information and coding schemes in a given Studio workspace
setMethod("get_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_ids = NULL,
                   # unit_keys = NULL,
                   metadata = TRUE,
                   unit_definition = FALSE,
                   coding_scheme = FALSE) {
            get_files(
              workspace = workspace,
              id = unit_ids,
              type = "unit",
              list_fun = list_units,
              get_fun = function(workspace, id) get_unit(workspace = workspace,
                                                         unit_id = id,
                                                         metadata = metadata,
                                                         unit_definition = unit_definition,
                                                         coding_scheme = coding_scheme)
            )
          })

# setMethod("get_units",
#           signature = signature(workspace = "WorkspaceTestcenter"),
#           function(workspace,
#                    unit_ids = NULL,
#                    unit_keys = NULL,
#                    metadata = TRUE,
#                    unit_definition = FALSE,
#                    coding_scheme = FALSE,
#                    verbose = FALSE) {
#             get_files(
#               workspace = workspace,
#               id = id,
#               type = "unit",
#               list_fun = listUnits,
#               get_fun = function(workspace, id) get_unit(workspace = workspace,
#                                                        id = id,
#                                                        prepare = prepare)
#             ) %>%
#               dplyr::rename(unitname = id)
#           })
