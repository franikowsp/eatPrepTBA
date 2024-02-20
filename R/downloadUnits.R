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
setGeneric("downloadUnits", function(workspace, units, path, ...) {
  cli_setting()
  standardGeneric("downloadUnits")
})

#' @describeIn uploadFile Upload a file in a defined workspace
setMethod("downloadUnits",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, units,
                   path,
                   overwrite = TRUE,
                   addPlayers = TRUE,
                   addTestTakersReview = 0,
                   addTestTakersMonitor = 0,
                   addTestTakersHot = 0,
                   passwordLess = FALSE,
                   bookletSettings = list()) {

            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers <- c(
              Authorization = workspace@login@token
            )

            unit_keys <- listUnits(workspace)
            unit_ids <- unit_keys[units]

            unitIdList <-
              unit_ids %>%
              unname() %>%
              as.list()

            body <- list(
              unitIdList = unitIdList,
              addPlayers = addPlayers,
              addTestTakersReview = addTestTakersReview,
              addTestTakersMonitor = addTestTakersMonitor,
              addTestTakersHot = addTestTakersHot,
              passwordLess = passwordLess,
              bookletSettings = bookletSettings
            ) %>%
              jsonlite::toJSON(auto_unbox = TRUE)

            # Item data and meta data
            request_download <- httr::GET(
              url = glue::glue(
                "{domain}/workspace/{ws_id}/download/{body}"
              ),
              httr::add_headers(headers),
              httr::write_disk(path = path, overwrite = overwrite)
            )

            zip::zip_list(path)
            # TODO: Weiter ausbauen
            # if (request_upload$status_code == 200) {
            #   notifications <- httr::content(request_upload)
            #
            #   notifications %>%
            #     purrr::iwalk(
            #       function(x, i) {
            #         cli::cli_h3(i)
            #         x %>%
            #           purrr::iwalk(function(x, i) {
            #             switch(i,
            #                    "success" = purrr::walk(x, cli::cli_alert_success),
            #                    "info" = purrr::walk(x, cli::cli_alert_info),
            #                    "warning" = purrr::walk(x, cli::cli_alert_warning),
            #                    "error" = purrr::walk(x, cli::cli_alert_danger)
            #             )
            #           })
            #       }
            #     )
            # } else {
            #   cli::cli_alert("{request_upload$status_code}: {content(request_upload, as = 'text')}")
            # }

            # return(invisible(NULL))
          })
