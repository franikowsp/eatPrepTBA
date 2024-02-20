#' Get a booklet
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve booklet from the API.
#' @param id Character. Name of the booklet to be retrieved.
#'
#' @description
#' This function only returns the booklet information for a single booklet. To retrieve multiple booklets, use [getBooklets()].
#'
#' @return A tibble.
#'
#' @examples
#' @aliases
#' getBooklet,WorkspaceTestcenter-method
setGeneric("getBooklet", function(workspace, id) {
  cli_setting()
  standardGeneric("getBooklet")
})

#' @describeIn getBooklet Get a booklet in a defined workspace
setMethod("getBooklet",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, id) {

            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            # Read unit XML -----------------------------------------------------------
            # Item data and meta data
            request_xml <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/file/Booklet/{id}.xml"
            ),
            config = httr::add_headers(.headers = headers)
            )

            if (request_xml$status_code == 200) {
              response_xml <-
                httr::content(request_xml,
                              type = "application/xml",
                              encoding = "UTF-8") %>%
                xml2::as_list() %>%
                purrr::pluck("Booklet") %>%
                tibble::enframe() %>%
                tidyr::pivot_wider()
            } else {
              response_xml <- tibble::tibble()
            }

            response_tbl <-
              response_xml

            return(response_tbl)
          })
