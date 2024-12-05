#' List unit files
#'
#' @param workspace [Workspace-class] or [WorkspaceGroupStudio-class]. Workspace or workspace group information necessary to retrieve unit list from the API.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_units,WorkspaceStudio-method,WorkspaceGroupStudio-method,WorkspaceTestcenter-method
setGeneric("list_units", function(workspace, ...) {
  args <- rlang::list2(...)

  standardGeneric("list_units")
})

#' @describeIn list_units List all units in a given IQB Studio workspace
#' @details
#' This function returns a list of all units in a given workspace.
setMethod("list_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "units")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Unit listing was not successful.",
                       default = tibble::tibble())

            resp %>%
              purrr::map(function(unit) {
                list(
                  unit_id = unit[["id"]],
                  unit_key = unit[["key"]],
                  unit_label = unit[["name"]]
                )
              })
          })

#' @describeIn list_units List all units in a given IQB Studio workspace group
#' @details
#' This function returns a list of all units in a given workspace group.
setMethod("list_units",
          signature = signature(workspace = "WorkspaceGroupStudio"),
          function(workspace, flat = FALSE) {
            units_list <-
              workspace@ws_list %>%
              purrr::map(function(ws) {
                ws_out <- list(
                  ws_id = ws@ws_id,
                  ws_label = ws@ws_label,
                  units = list_units(ws)
                )

                ws_out
              })

            if (flat) {
              units_list %>%
                purrr::map("units") %>%
                purrr::list_flatten()
            } else {
              units_list
            }
          })

#' @describeIn list_units List all units in a given IQB Testcenter workspace
#' @details
#' This function serves as a wrapper for [list_files()].
setMethod("list_units",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            ws_units <- list_files(workspace, type = "Unit")

            ws_units %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
