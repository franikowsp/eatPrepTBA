#' Get system check results
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved.
#'
#' @description
#' This function returns system check results for the selected system check instance.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getResponses,WorkspaceTestcenter-method
setGeneric("getSystemCheck", function(workspace, groups) {
  standardGeneric("getSystemCheck")
})

#' @describeIn getResponses Get responses of a defined workspace
setMethod("getSystemCheck",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token,
              Accept = "text/csv"
            )

            params = list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/report/sys-check"
            ),
            httr::add_headers(.headers=headers),
            query = params
            )

            # TODO: CSV muss noch richtig getrennt werden
            if (request_json$status_code == 200) {
              response_json <-
                httr::content(request_json,
                              type = "text",
                              encoding = "UTF-8") %>%
                readr::read_delim(delim  = ";")
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
