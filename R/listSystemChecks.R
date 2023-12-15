#' List system check files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve system check file list from the API.
#'
#' @description
#' Please ....
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' listFiles,WorkspaceTestcenter-method
setGeneric("listSystemChecks", function(workspace, ...) {
  standardGeneric("listSystemChecks")
})

#' @describeIn listFiles List all files in a defined workspace
#'
#' @param type Character (optional). Type of the files to retrieve from the API. If no type is specified, all files are listed.
setMethod("listSystemChecks",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            headers = c(
              AuthToken = workspace@login@token
            )

            domain <- workspace@login@domain
            ws_id <- workspace@id

            request <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/sys-check/reports/overview"
            ),
            httr::add_headers(.headers=headers)
            )

            httr::content(request) %>%
              purrr::list_transpose() %>%
              tibble::enframe() %>%
              tidyr::pivot_wider() %>%
              dplyr::select(
                id, count, label
              ) %>%
              tidyr::unnest(c(id, count, label))
          })
