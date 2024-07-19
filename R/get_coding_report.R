#' Get coding report
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve the coding report.
#'
#' @description
#' This function returns the coding report of the given IQB Studio workspace.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_coding_report,WorkspaceStudio-method
setGeneric("get_coding_report", function(workspace) {
  cli_setting()

  standardGeneric("get_coding_report")
})

#' @describeIn get_coding_report Upload a file in a defined workspace
setMethod("get_coding_report",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c(
                         "workspace",
                         ws_id,
                         "coding-report"
                       )) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Codebook could not be generated. Please check if you have already,
                     opened {.file {path}} (that migh cause the error).",
                       default = tibble::tibble())

            units <-
              list_units(workspace) %>%
              purrr::list_transpose() %>%
              tibble::as_tibble()

            resp %>%
              purrr::list_transpose() %>%
              tibble::as_tibble() %>%
              tidyr::separate(
                unit, into = c("unit_key", "unit_label"), sep = ":"
              ) %>%
              dplyr::mutate(
                ws_id = ws_id
              ) %>%
              dplyr::left_join(units) %>%
              # Reorder and rename entries
              dplyr::select(
                ws_id,
                unit_id,
                unit_key,
                unit_label,
                variable_id = variable,
                item,
                validation,
                coding_type = codingType
              )
          })
