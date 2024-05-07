#' Download units
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download files via the API.
#' @param path Character. Path for the zip file to be downloaded.
#'
#' @description
#' This function downloads units from the IQB Studio Lite.
#'
#' @return NULL
#' @export
#'
#' @examples
#' @aliases
#' uploadFile,WorkspaceStudio-method
setGeneric("getCodingReport", function(workspace, units, path, ...) {
  cli_setting()
  standardGeneric("getCodingReport")
})

#' @describeIn uploadFile Upload a file in a defined workspace
setMethod("getCodingReport",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {

            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers <- c(
              Authorization = workspace@login@token
            )

            unit_keys <-
              listUnits(workspace) %>%
              tibble::enframe(name = "unit_key", value = "unit_id")

            # Item data and meta data
            request_coding_report <- httr::GET(
              url = glue::glue(
                "{domain}/workspace/{ws_id}/coding-report"
              ),
              httr::add_headers(headers)
            )

            if (httr::status_code(request_coding_report) == 200) {
              httr::content(request_coding_report) %>%
                purrr::map(tibble::as_tibble) %>%
                purrr::reduce(dplyr::bind_rows) %>%
                tidyr::separate(
                  unit, into = c("unit_key", "unit_label"), sep = ":"
                ) %>%
                dplyr::mutate(
                  ws_id = ws_id
                ) %>%
                dplyr::left_join(unit_keys)
            } else {
              tibble::tibble()
            }
          })
