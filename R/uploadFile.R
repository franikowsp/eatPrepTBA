#' Upload file
#'
#' @param workspace [Workspace-class]. Workspace information necessary to upload file via the API.
#' @param path Character. Path to the file to be uploaded.
#'
#' @description
#' This function uploads a file to the IQB Testcenter.
#'
#' @return NULL
#' @export
#'
#' @examples
#' @aliases
#' uploadFile,Workspace-method
setGeneric("uploadFile", function(workspace, path) {
  cli_setting()
  standardGeneric("uploadFile")
})

#' @describeIn uploadFile Upload a file in a defined workspace
setMethod("uploadFile",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, path) {

            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            body = list(
              'fileforvo' = httr::upload_file(path)
            )

            # Item data and meta data
            request_upload <- httr::POST(
              url = glue::glue(
                "{domain}/workspace/{ws_id}/file"
              ),
              body = body,
              httr::add_headers(headers),
              encode = 'multipart'
            )

            if (request_upload$status_code %in% c(201, 207)) {
              notifications <- httr::content(request_upload)

              notifications %>%
                purrr::iwalk(
                  function(x, i) {
                    cli::cli_h3(i)
                    x %>%
                      purrr::iwalk(function(x, i) {
                        switch(i,
                               "success" = purrr::walk(x, cli::cli_alert_success),
                               "info" = purrr::walk(x, cli::cli_alert_info),
                               "warning" = purrr::walk(x, cli::cli_alert_warning),
                               "error" = purrr::walk(x, cli::cli_alert_danger)
                        )
                      })
                  }
                )
            } else {
              response_status <- request_upload$status_code
              response_upload <- httr::content(request_upload, as = 'text')

              cli::cli_alert("{response_status}: {response_upload}")
            }

            return(invisible(NULL))
          })
