#' Get multiple units with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param units Tibble. Recently queried units of this workspace that should be updated. Please note that units that are no longer in the workspace will automatically be deleted by the procedure.
#' @param metadata Logical. Should the metadata be added? Defaults to `TRUE`.
#' @param unit_definition Logical. Should the unit definition be added? Defaults to `FALSE`. This function also infers the pages of the variables from the unit definition.
#'
#' @description
#' This function returns the unit information for multiple units.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_units,WorkspaceTestcenter-method,WorkspaceStudio-method
setGeneric("get_units", function(workspace,
                                 units = NULL,
                                 metadata = TRUE,
                                 unit_definition = FALSE) {
  standardGeneric("get_units")
})

#' @describeIn get_units Get multiple unit information and coding schemes in a given Studio workspace
setMethod("get_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   units = NULL,
                   metadata = TRUE,
                   unit_definition = FALSE) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            ws_label <- workspace@ws_label

            # Workspace information
            ws_settings <- get_settings(workspace)
            ws_states <- ws_settings$states[[1]]

            ws_info <-
              ws_settings %>%
              dplyr::select(
                ws_id, ws_label, wsg_id, wsg_label
              )

            units_old <- units

            if (!is.null(units_old)) {
              units_ws_id <- unique(units_old$ws_id)

              if (length(units_ws_id) != 1 || ws_id != units_ws_id) {
                not_in_ws <- setdiff(units_ws_id, ws_id)
                n_not_in_ws <- length(not_in_ws)

                cli::cli_abort("Units to should only be retrieved from workspace {.ws-id {ws_id}} {.ws-label {ws_label}}.
                               There are units from {n_not_in_ws} other workspace{?s}: {.ws-id {not_in_ws}}.
                               Please remove these units from these workspaces before updating or retrieve the units afresh",
                               wrap = TRUE)
              }
            }

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspaces", ws_id, "units", "properties")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp_metadata <-
              run_safe(run_req,
                       error_message = "Units could not be retrieved.",
                       default = tibble::tibble())

            if (length(resp_metadata) != 0) {
              units_new <-
                resp_metadata %>%
                purrr::map(read_unit, .progress = "Reading units") %>%
                dplyr::bind_rows() %>%
                dplyr::select(any_of(c(
                  unit_id = "id",
                  unit_key = "key",
                  unit_label = "name",
                  group_name = "groupName",
                  state_id = "state",
                  description = "description",
                  unit_variables = "variables",
                  unit_metadata = "metadata",
                  coding_scheme = "scheme",
                  player = "player",
                  editor = "editor",
                  schemer = "schemer",
                  scheme_type = "schemeType",
                  last_change_definition = "lastChangedDefinition",
                  last_change_definition_user = "lastChangedDefinitionUser",
                  last_change_scheme = "lastChangedScheme",
                  last_change_scheme_user = "lastChangedSchemeUser",
                  last_change_metadata = "lastChangedMetadata",
                  last_change_metadata_user = "lastChangedMetadataUser"
                ))) %>%
                dplyr::bind_cols(ws_info) %>%
                dplyr::relocate(
                  names(ws_info),
                  .before = "unit_id"
                ) %>%
                dplyr::mutate(
                  # Fresh units might not have a state
                  state_id = ifelse("state_id" %in% names(.), state_id, NA)
                )

              if (!is.null(ws_states)) {
                units_new <-
                  units_new %>%
                  dplyr::left_join(ws_states, by = dplyr::join_by("state_id"), copy = TRUE) %>%
                  dplyr::relocate(
                    names(ws_states),
                    .after = "group_name"
                  )
              }

              # units_new %>% dplyr::mutate(no = seq_along(ws_id)) %>% dplyr::filter(unit_key == "EL_FF02") %>% .$no

              if (!is.null(units_old)) {
                # Filter for units that are available (automatic update)
                units_old <-
                  units_old %>%
                  dplyr::semi_join(units_new %>% dplyr::select(unit_id), by = dplyr::join_by("unit_id"))

                units_change_old <-
                  units_old %>%
                  get_last_change() %>%
                  dplyr::rename(last_change_old = last_change)

                units_change_new <-
                  units_new %>%
                  get_last_change() %>%
                  dplyr::rename(last_change_new = last_change)

                update_units <-
                  units_change_new %>%
                  dplyr::left_join(units_change_old, by = dplyr::join_by("unit_id")) %>%
                  dplyr::filter(
                    # Units that were not in the workspace or units that have newer schemes
                    is.na(last_change_old) | (!is.na(last_change_old) & (last_change_new > last_change_old))
                  ) %>%
                  dplyr::select(unit_id)

                n_updates <- nrow(update_units)

                cli::cli_alert_info("Found {cli::no(n_updates)} update{?s} in units.")
              } else {
                update_units <-
                  units_new %>%
                  dplyr::distinct(unit_id)
              }

              units_to_update <-
                units_new %>%
                dplyr::semi_join(update_units, by = dplyr::join_by("unit_id"))

              if (!is.null(units_old)) {
                units_no_update <-
                  units_old %>%
                  dplyr::anti_join(update_units, by = dplyr::join_by("unit_id"))
              }

              if (metadata | (!is.null(units_old) && tibble::has_name(units_no_update, "items_list"))) {
                units_to_update <- read_metadata(units = units_to_update)

                if (!is.null(units_old) && tibble::has_name(units_no_update, "unit_metadata")) {
                  cli::cli_alert_info("Reading metadata of supplied units.")

                  units_no_update <-
                    read_metadata(units = units_no_update) %>%
                    dplyr::select(-dplyr::any_of(c("unit_metadata")))
                }
              }

              if (unit_definition | (!is.null(units_old) && tibble::has_name(units_no_update, "unit_definition"))) {
                units_to_update <- read_definition(units = units_to_update)

                if (!is.null(units_old) && !tibble::has_name(units_no_update, "unit_definition")) {
                  cli::cli_alert_info("Adding unit definitions of supplied units.")

                  units_no_update <- read_definition(units = units_no_update)
                }
              }

              units_final <-
                units_to_update %>%
                dplyr::arrange(unit_id)

              if (!is.null(units_old)) {
                units_final <-
                  units_no_update %>%
                  dplyr::bind_rows(units_to_update) %>%
                  dplyr::arrange(unit_id)
              }

            } else {
              units_final <- tibble::tibble()

              cli::cli_alert_info("No units to be retrieved workspace from {.ws-id {workspace@ws_id}}: {.ws {workspace@ws_label}}")
            }

            return(units_final)
          })

read_unit <- function(unit) {
  unit_prepared <-
    unit %>%
    purrr::discard(names(.) %in% c("metadata", "variables")) %>%
    purrr::compact() %>%
    tibble::as_tibble()

  metadata <-
    unit %>%
    purrr::pluck("metadata")

  variables <-
    unit %>%
    purrr::pluck("variables")

  if (length(variables) > 0) {
    variables <-
      variables %>%
      purrr::list_transpose() %>%
      tibble::as_tibble() %>%
      dplyr::select(
        dplyr::any_of(c(
          "variable_id" = "alias",
          "variable_ref" = "id",
          "variable_page" = "page",
          "variable_type" = "type",
          "variable_format" = "format",
          "variable_values" = "values",
          "variable_multiple" = "multiple",
          "variable_nullable" = "nullable",
          "values_complete" = "valuesComplete",
          "value_position_labels" = "valuePositionLabels"
        ))
      ) %>%
      dplyr::mutate(
        variable_values = purrr::map(variable_values, function(x) {
          if (length(x) == 0) {
            out <- tibble::tibble(value = NA, value_label = NA)
          } else {
            out <- x %>%
              purrr::list_transpose() %>%
              tibble::as_tibble() %>%
              dplyr::rename(dplyr::any_of(c(
                "value_label" = "label"
              )))
          }
        })
      )
  }

  unit_prepared %>%
    dplyr::mutate(
      metadata = list(metadata),
      variables = list(variables)
    )
}

get_definition <- function(unit_key, ws_id, unit_id) {
  run_req_definition <- function() {
    base_req(method = "GET",
             endpoint = c("workspaces", ws_id, "units", unit_id, "definition")) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }

  message <- glue::glue("Unit definition for {{.unit-key {unit_key}}} could not be retrieved.
               Please check: {{.href https://www.iqb-studio.de/#/a/{unit_id}/{unit_id}}}.")

  # Todo: Check if this could also be in a loop somehow?
  resp_definition <-
    run_safe(run_req_definition,
             error_message = message)

  if (!is.null(resp_definition)) {
    unit <- prepare_definition(resp_definition)
  }
}

read_definition <- function(units) {
  unit_keys <- units$unit_key

  if (length(unit_keys) > 0) {
    units %>%
      dplyr::mutate(
        unit_definition = purrr::pmap(
          list(unit_key, ws_id, unit_id),
          get_definition,
          .progress = list(
            type ="custom",
            extra = list(
              unit_keys = unit_keys
            ),
            format = "Preparing unit definition for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
            format_done = "Prepared {cli::pb_total} unit definition{?s} in {cli::pb_elapsed}.",
            clear = FALSE
          ))
      ) %>%
      tidyr::unnest(unit_definition)
  } else {
    units
  }
}

get_last_change <- function(units) {
  units %>%
    dplyr::select(unit_id,
                  last_change_definition,
                  last_change_metadata,
                  last_change_scheme) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("last_change"), lubridate::as_datetime),
      last_change = max(dplyr::c_across(dplyr::starts_with("last_change")))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(unit_id, last_change)
}

# setMethod("get_units",
#           signature = signature(workspace = "WorkspaceTestcenter"),
#           function(workspace,
#                    unit_ids = NULL,
#                    unit_keys = NULL,
#                    metadata = TRUE,
#                    unit_definition = FALSE,
#                    coding_scheme = FALSE,
#                    verbose = FALSE) {
#             get_files(
#               workspace = workspace,
#               id = id,
#               type = "unit",
#               list_fun = listUnits,
#               get_fun = function(workspace, id) get_unit(workspace = workspace,
#                                                        id = id,
#                                                        prepare = prepare)
#             ) %>%
#               dplyr::rename(unitname = id)
#           })
