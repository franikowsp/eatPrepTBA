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

            cli::cli_h2("Preparing {.unit-label unit} database")

            cli::cli_h3("Retrieving {.ws-label workspace} settings")

            ws_settings <- get_settings(workspace)

            ws_states <-
              ws_settings %>%
              dplyr::select(ws_id, states) %>%
              tidyr::unnest(states)

            ws_info <-
              ws_settings %>%
              dplyr::select(
                ws_id, ws_label, wsg_id, wsg_label
              )

            cli::cli_text("Prepared settings for {length(ws_id)} {.ws-label workspace{?s}}")

            units_old <- units

            run_req <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c("workspaces", ws_id, "units", "properties")) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json(check_type = TRUE)
              }

              return(req)
            }

            cli::cli_h3("Retrieving {.unit-label units}")

            resp_metadata <-
              purrr::map(ws_id,
                         function(ws_id) {
                           run_safe(run_req(ws_id),
                                    error_message = "Units could not be retrieved.",
                                    default = list()) %>%
                             read_units(ws_id = ws_id)
                         }) %>%
              dplyr::bind_rows()

            units_new <-
              resp_metadata %>%
              dplyr::left_join(ws_info, by = c("ws_id")) %>%
              dplyr::relocate(
                names(ws_info),
                .before = "unit_id"
              ) %>%
              dplyr::mutate(
                # Fresh units might not have a state
                state_id = ifelse("state_id" %in% names(.), state_id, NA)
              ) %>%
              dplyr::left_join(ws_states, by = dplyr::join_by("ws_id", "state_id")) %>%
              dplyr::relocate(
                names(ws_states),
                .after = "group_name"
              )

            if (!is.null(units_old)) {
              # Filter for units that are available (automatic update)
              units_old <-
                units_old %>%
                dplyr::semi_join(units_new %>% dplyr::select(ws_id, unit_id),
                                 by = dplyr::join_by("ws_id", "unit_id"))

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
              units_to_update <- read_definition(units = units_to_update, base_req = base_req)

              if (!is.null(units_old) && !tibble::has_name(units_no_update, "unit_definition")) {
                cli::cli_alert_info("Adding unit definitions of supplied units.")

                units_no_update <- read_definition(units = units_no_update, base_req = base_req)
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

            attr(units_final, "ws_settings") <- ws_settings
            class(units_final) <- c("tba_units", class(units_final))

            return(units_final)
          })

read_units <- function(ws, ws_id) {
  if (length(ws) == 0) {
    return(tibble::tibble(ws_id = ws_id))

    cli::cli_alert_info("No units to be retrieved from {.ws-id {workspace@ws_id}}: {.ws {workspace@ws_label}}")
  }

  ws %>%
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
    dplyr::mutate(ws_id)
}

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
    variables_prep <-
      variables %>%
      purrr::list_transpose() %>%
      tibble::as_tibble()

    # For legacy reasons
    if (!tibble::has_name(variables_prep, "alias")) {
      variables_prep <-
        variables_prep %>%
        dplyr::mutate(
          alias = id
        )
    }

    variables <-
      variables_prep %>%
      dplyr::mutate(
        alias = ifelse(is.na(alias), id, alias)
      ) %>%
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
          "variable_values_complete" = "valuesComplete",
          "variable_value_position_labels" = "valuePositionLabels"
        ))
      ) %>%
      dplyr::mutate(
        # This could be a bug when calling units
        dplyr::across(dplyr::any_of(c("variable_multiple", "variable_nullable", "variable_values_complete")),
                      list_to_logical),
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

list_to_logical <- function(x) {
  purrr::map_lgl(x, function(x) {
    if (is.null(x)) {
      return(NA)
    }

    x %>%
      stringr::str_to_upper() %>%
      as.logical()
  })
}

get_definition <- function(unit_key, ws_id, unit_id, base_req) {
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

read_definition <- function(units, base_req) {
  unit_keys <- units$unit_key

  if (length(unit_keys) > 0) {
    units %>%
      dplyr::mutate(
        unit_definition = purrr::pmap(
          list(unit_key, ws_id, unit_id),
          function(unit_key, ws_id, unit_id) get_definition(unit_key, ws_id, unit_id, base_req),
          .progress = list(
            type ="custom",
            extra = list(
              unit_keys = pad_ids(unit_keys)
            ),
            format = "Preparing unit definitions for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
            format_done = "Prepared unit definitions of {cli::pb_total} {.unit-label unit{?s}} of {cli::pb_extra$n_ws_ids} {.ws-label workspace{?s}} metadata in {cli::pb_elapsed}.",
            format_failed = "Failed at preparing unit definition for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}}",
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
