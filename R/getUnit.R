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
#' getUnit,WorkspaceTestcenter-method,WorkspaceStudio-method
setGeneric("getUnit", function(workspace, id, ...) {
  standardGeneric("getUnit")
})

#' @describeIn getUnit Get unit information and coding scheme in a defined workspace
setMethod("getUnit",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, id, prepare = TRUE) {
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

            if (prepare) {
              response_tbl %>%
                dplyr::mutate(
                  xml = purrr::map(xml, prepareUnitXml),
                  vocs = purrr::map(vocs, prepareUnitVocs)
                )
            } else {
              return(response_tbl)
            }
          })

#' @describeIn getUnit Get unit information and coding scheme in a defined workspace
setMethod("getUnit",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, id) {
            headers = c(
              Authorization = workspace@login@token,
              "app-version" = workspace@login@version
            )

            domain <- workspace@login@domain
            ws_id <- workspace@id
            ws_label <- workspace@label

            # Read unit XML -----------------------------------------------------------
            # Item data and meta data
            request <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/{id}/metadata"
            ),
            config = httr::add_headers(.headers = headers)
            )

            if (request$status_code == 200) {
              unit <- httr::content(request)

              unit_meta <-
                purrr::discard(unit, names(unit) == "metadata") %>%
                purrr::map(function(x) {
                  if (length(x) == 0) {
                    ""
                  } else {
                    x
                  }
                }) %>%
                tibble::as_tibble()

              # Unit-Metadaten
              if (!is.null(unit$metadata$profiles)) {
                unit_profiles <-
                  unit$metadata$profiles %>%
                  purrr::keep("isCurrent") %>%
                  purrr::pluck(1, "entries") %>%
                  purrr::map(function(x) unlist(x) %>% tibble::enframe() %>% tidyr::pivot_wider()) %>%
                  purrr::reduce(dplyr::bind_rows) %>%
                  dplyr::select(label.value, value.id, valueAsText.value)
              } else {
                unit_profiles <-
                  tibble::tibble()
              }

              # Item-Metadaten
              items_meta <-
                unit$metadata$items %>%
                purrr::map(function(x) {
                  purrr::discard(x, .p = names(x) == "profiles") %>%
                    tibble::as_tibble()
                }) %>%
                tibble::enframe(name = "item") %>%
                tidyr::unnest(value)

              if (!is.null(unit$metadata$items %>% purrr::map("profiles") %>% purrr::reduce(c))) {
                items_profiles <-
                  unit$metadata$items %>%
                  purrr::map(function(x) {
                    x %>%
                      purrr::pluck("profiles") %>%
                      purrr::keep("isCurrent")
                  }) %>%
                  purrr::map(function(x) {
                    entries <- x %>%
                      purrr::pluck(1, "entries") %>%
                      purrr::map(function(x) {
                        unlist(x) %>%
                          as.list() %>%
                          tibble::enframe() %>%
                          tidyr::unnest(value) %>%
                          tidyr::pivot_wider(values_fn = function(x) stringr::str_c(x, collapse = ";"))
                      }) %>%
                      purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())}) %>%
                  tibble::enframe(name = "item") %>%
                  tidyr::unnest(value) %>%
                  dplyr::select(item, label.value, value.id, valueAsText.value) %>%
                  dplyr::mutate(
                    dplyr::across(c(value.id, valueAsText.value), function(x) stringr::str_split(x, ";"))
                  ) %>%
                  tidyr::unnest(c(value.id, valueAsText.value))
              } else {
                items_profiles <-
                  tibble::tibble()
              }

              response <-
                unit_meta %>%
                dplyr::mutate(
                  unit_profiles = list(unit_profiles),
                  items_meta = list(items_meta),
                  items_profiles = list(items_profiles),
                ) %>%
                dplyr::rename(
                  unit_id = id
                )
            } else {
              response <- tibble::tibble()
            }

            return(response)
          })
