#' Get testtakers file
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve testtakers file from the API.
#' @param id Character. Name of the testtakers file to be retrieved.
#'
#' @description
#' This function only returns the testtakers information for a single testtakers file To retrieve multiple testtakers files, use [getTesttakers()].
#'
#' @return A tibble.
#'
#' @examples
#' @aliases
#' getTesttaker,Workspace-method
setGeneric("getTesttaker", function(workspace, id) {
  standardGeneric("getTesttaker")
})

#' @describeIn getTesttaker Get testtaker information in a in a defined workspace
setMethod("getTesttaker",
          signature = signature(workspace = "Workspace"),
          function(workspace, id) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            # Read unit XML -----------------------------------------------------------
            # Item data and meta data
            request_xml <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/file/Testtakers/{id}.xml"
            ),
            config = httr::add_headers(.headers=headers)
            )

            if (request_xml$status_code == 200) {
              response_xml <-
                httr::content(request_xml,
                              type = "application/xml",
                              encoding = "UTF-8") %>%
                xml2::as_list() %>%
                purrr::pluck("Testtakers") %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(values_fn = list)
            } else {
              response_xml <- tibble::tibble()
            }

            response_tbl <-
              response_xml

            return(response_tbl)
          })
