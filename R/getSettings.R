#' Get workspace settings
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [getUnits()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getSettings,WorkspaceStudio-method
setGeneric("getSettings", function(workspace, metadata) {
  standardGeneric("getSettings")
})

#' @describeIn getUnit Get unit information and coding scheme in a defined workspace
setMethod("getSettings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, metadata = TRUE) {
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
              "{domain}/workspace/{ws_id}"
            ),
            config = httr::add_headers(.headers = headers)
            )

            if (request$status_code == 200) {
              ws <- httr::content(request)

              # General metadata
              ws_meta <-
                purrr::discard(ws, names(ws) == "settings") %>%
                purrr::map(function(x) {
                  if (length(x) == 0) {
                    ""
                  } else {
                    x
                  }
                }) %>%
                tibble::as_tibble()

              # Settings
              if (!is.null(ws$settings)) {
                settings <- ws$settings

                # States
                if (!is.null(settings$states)) {
                  ws_states <-
                    ws$settings$states %>%
                    purrr::map(function(x) unlist(x) %>% tibble::enframe() %>% tidyr::pivot_wider()) %>%
                    purrr::reduce(dplyr::bind_rows)
                } else {
                  ws_states <-
                    tibble::tibble()
                }

                # Unit groups
                if (!is.null(settings$unitGroups)) {
                  ws_groups <-
                    ws$settings$unitGroups %>%
                    unlist()
                } else {
                  ws_groups <-
                    tibble::tibble()
                }

                # Rest
                ws_rest <-
                  purrr::discard(ws$settings, names(ws$settings) %in% c("states", "unitGroups")) %>%
                  tibble::as_tibble()

                ws_settings <-
                  ws_rest %>%
                  dplyr::mutate(
                    states = list(ws_states),
                    groups = list(ws_groups)
                  )
              } else {
                ws_settings <- tibble::tibble()
              }

              ws_info <-
                ws_meta %>%
                dplyr::bind_cols(ws_settings)

            } else {
              ws_info <- tibble::tibble()
            }

            if (metadata) {
              if (!is.null(ws$settings$itemMDProfile)) {
                itemMetadata <- getMetadataProfile(ws$settings$itemMDProfile)
              } else {
                itemMetadata <- tibble::tibble()
              }
              if (!is.null(ws$settings$itemMDProfile)) {
                unitMetadata <- getMetadataProfile(ws$settings$unitMDProfile)
              } else {
                unitMetadata <- tibble::tibble()
              }

              ws_info <-
                ws_info %>%
                dplyr::mutate(
                  itemMetadata = list(itemMetadata),
                  unitMetadata = list(unitMetadata),
                )
            }

            return(ws_info)
          })
