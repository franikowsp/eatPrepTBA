#' Get workspace settings
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param metadata Logical. Should the workspace metadata, i.e., the item and unit metadata profiles, be prepared? Defaults to `TRUE`.
#'
#' @description
#' This function returns the workspace settings for a single workspace.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_settings,WorkspaceStudio-method
setGeneric("get_settings", function(workspace, metadata = TRUE) {
  standardGeneric("get_settings")
})

#' @describeIn get_settings Get unit information and coding scheme in a defined workspace
setMethod("get_settings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, metadata = TRUE) {
            # TODO: This function should only run once (for access_workspace or for access_workspace_group)
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            wsg_id <- workspace@wsg_id

            # General metadata
            ws_general <-
              tibble::tibble(
                ws_id = as.integer(ws_id),
                ws_label = workspace@ws_label,
                wsg_id = as.integer(wsg_id),
                wsg_label = workspace@wsg_label,
              )

            # Workspace unit groups and default settings
            run_req_ws <- function() {
              base_req(method = "GET",
                       endpoint = c("workspaces", ws_id)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp_ws <-
              run_safe(run_req_ws,
                       error_message = "Workspace settings could not be retrieved.",
                       default = list())

            if (!is.null(resp_ws$settings)) {
              # Default settings
              ws_defaults <-
                resp_ws$settings %>%
                purrr::discard(names(.) %in% c("unitGroups", "states")) %>%
                purrr::compact() %>%
                tibble::as_tibble() %>%
                dplyr::rename(
                  # TODO: Add other entries to this list
                  dplyr::any_of(c(
                    default_editor = "defaultEditor",
                    default_player = "defaultPlayer",
                    item_md_profile = "itemMDProfile",
                    unit_md_profile = "unitMDProfile",
                    stable_modules_only = "stableModulesOnly"
                  ))
                )

              ws_general <-
                ws_general %>%
                dplyr::bind_cols(ws_defaults)

              # Unit groups
              if (!is.null(resp_ws$settings$unitGroups)) {
                ws_groups <-
                  resp_ws$settings$unitGroups %>%
                  unlist() %>%
                  sort()

                ws_general <-
                  ws_general %>%
                  dplyr::mutate(groups = list(ws_groups))
              }
            }

            # Workspace group settings for states
            run_req_wsg <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace-groups", wsg_id)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp_wsg <-
              run_safe(run_req_wsg,
                       error_message = "Workspace group settings could not be retrieved.",
                       default = list())

            if (!is.null(resp_wsg$settings)) {
              wsg_settings <- resp_wsg$settings

              # States
              if (!is.null(wsg_settings$states) & length(wsg_settings$states) != 0) {
                wsg_states <-
                  wsg_settings$states %>%
                  purrr::map(function(x) unlist(x) %>% tibble::enframe() %>% tidyr::pivot_wider()) %>%
                  purrr::reduce(dplyr::bind_rows) %>%
                  dplyr::rename(
                    state_id = id,
                    state_label = label,
                    state_color = color
                  )

                ws_general <-
                  ws_general %>%
                  dplyr::mutate(
                    states = list(wsg_states)
                  )
              }
            }

            # Add metadata profiles
            if (metadata) {
              if (!is.null(resp_ws$settings$itemMDProfile)) {
                item_metadata <- get_metadata_profile(resp_ws$settings$itemMDProfile)
              } else {
                item_metadata <- tibble::tibble()
              }
              if (!is.null(resp_ws$settings$itemMDProfile)) {
                unit_metadata <- get_metadata_profile(resp_ws$settings$unitMDProfile)
              } else {
                unit_metadata <- tibble::tibble()
              }

              ws_general <-
                ws_general %>%
                dplyr::mutate(
                  item_metadata = list(item_metadata),
                  unit_metadata = list(unit_metadata),
                )
            }

            return(ws_general)
          })
