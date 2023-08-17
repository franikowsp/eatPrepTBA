#' Get a unit with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param id Character. Name of the unit to be retrieved.
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [getUnits()].
#'
#' @return A tibble.
#'
#' @examples
#' @aliases
#' getUnit,Workspace-method
setGeneric("getUnit", function(workspace, id) {
  standardGeneric("getUnit")
})

#' @describeIn getUnit Get unit information and coding scheme in a defined workspace
setMethod("getUnit",
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
              "{domain}/workspace/{ws_id}/file/Unit/{id}.xml"
            ),
            config = httr::add_headers(.headers = headers)
            )

            if (request_xml$status_code == 200) {
              response_xml <-
                httr::content(request_xml,
                              type = "application/xml",
                              encoding = "UTF-8") %>%
                xml2::as_list() %>%
                purrr::pluck("Unit") %>%
                tibble::enframe() %>%
                tidyr::pivot_wider()
            } else {
              response_xml <- tibble::tibble()
            }

            # Reat unit VOCS ----------------------------------------------------------
            # Coding scheme
            request_vocs <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/file/Resource/{id}.vocs"
            ),
            config = httr::add_headers(.headers=headers)
            )

            if (request_vocs$status_code == 200) {
              response_vocs <-
                httr::content(request_vocs,
                              type = "application/json",
                              encoding = "UTF-8") %>%
                purrr::pluck("variableCodings") %>%
                tibble::enframe()
            } else {
              response_vocs <- tibble::tibble()
            }


            response_tbl <-
              tibble::tibble(
                xml = list(response_xml),
                vocs = list(response_vocs)
              )

            return(response_tbl)
          })
