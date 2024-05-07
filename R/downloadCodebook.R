#' Download units
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download codebook via the API.
#' @param path Character. Path for the codebook file to be downloaded.
#'
#' @description
#' This function downloads units from the IQB Studio Lite.
#'
#' @return NULL
#' @export
#'
#' @examples
#' @aliases
#' downloadCodebook,WorkspaceStudio-method
setGeneric("downloadCodebook", function(workspace, units, path, ...) {
  cli_setting()
  standardGeneric("downloadCodebook")
})

#' @describeIn downloadCodebook Upload a file in a defined workspace
setMethod("downloadCodebook",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, units,
                   path,
                   overwrite = TRUE,
                   format = "docx",
                   generalInstructions = TRUE,
                   onlyManual = TRUE,
                   closed = TRUE,
                   derived = TRUE) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers <- c(
              Authorization = workspace@login@token
            )

            unit_keys <- listUnits(workspace)
            unit_ids <- unit_keys[units]

            units_code <-
              unit_ids %>%
              paste0(collapse = ",")

            params <- list(
              format = format,
              generalInstructions = generalInstructions,
              onlyManual = onlyManual,
              closed = closed,
              derived = derived
            ) %>%
              purrr::map(function(x) if (is.logical(x)) as.character(x) %>% stringr::str_to_lower() else x)

            # Item data and meta data
            request_download <- httr::GET(
              url = glue::glue(
                "{domain}/download/docx/workspaces/{ws_id}/coding-book/{units_code}",
              ),
              httr::add_headers(headers),
              query = params,
              httr::write_disk(path = path, overwrite = overwrite)
            )
          })
