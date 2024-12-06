#' Get a unit with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_id Integer. ID of the unit to be retrieved (either `unit_id` or `unit_key` must be given; if these are discordant, only `unit_id` will be used).
#' @param unit_key Character. Name of the unit to be retrieved (either `unit_id` or `unit_key` must be given; if these are discordant, only `unit_id` will be used).
#' @param metadata Logical. Should the metadata be added? Defaults to `TRUE`.
#' @param unit_definition Logical. Should the unit definition be added? Defaults to `FALSE`.
#' @param coding_scheme Logical. Should the coding scheme be added? Defaults to `FALSE`.
#' @param verbose Logical. Should the function return additional messages. Defaults to `FALSE`.
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [get_units()].
#'
#' @return A tibble.
#'
#' @aliases
#' get_unit,WorkspaceTestcenter-method,WorkspaceStudio-method
#'
#' @keywords internal
setGeneric("get_unit", function(workspace,
                                unit_id,
                                # TODO: This should not stay for the Testcenter call ...
                                unit_key = NULL,
                                metadata = TRUE,
                                unit_definition = FALSE,
                                coding_scheme = FALSE,
                                verbose = FALSE) {
  cli_setting()

  standardGeneric("get_unit")
})

#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#'
#' @describeIn get_unit Get unit information and coding scheme in a defined workspace
setMethod("get_unit",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_id = NULL,
                   unit_key = NULL,
                   metadata = TRUE,
                   unit_definition = FALSE,
                   coding_scheme = FALSE,
                   verbose = FALSE) {
            if (is.null(unit_id) & is.null(unit_key)) {
              cli::cli_abort("Either {.unit-id unit_id} or
                             {.unit-key unit_key} must be specified.",
                             wrap = TRUE)
            }

            if (verbose & (!is.null(unit_id) & !is.null(unit_key))) {
              cli::cli_alert_info("Both {.unit-id unit_id} and
                                  {.unit-key unit_key} were given.
                                  Only {.unit-id unit_id} will be used.", wrap = TRUE)
            }

            if (!is.null(unit_key) & is.null(unit_id)) {
              unit_id <-
                list_units(workspace) %>%
                purrr::keep(function(x) x$unit_key == unit_key) %>%
                purrr::map_int("unit_id")
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, unit_id, "metadata")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp_metadata <-
              run_safe(run_req,
                       error_message = "Unit could not be retrieved.",
                       default = tibble::tibble())

            if (!is.null(resp_metadata)) {
              unit <-
                resp_metadata %>%
                purrr::discard(names(.) == "metadata") %>%
                purrr::compact() %>%
                tibble::as_tibble() %>%
                dplyr::rename(any_of(c(
                  unit_id = "id",
                  unit_key = "key",
                  unit_label = "name",
                  group_name = "groupName",
                  description = "description",
                  state = "state",
                  player = "player",
                  editor = "editor",
                  last_change_definition = "lastChangedDefinition",
                  last_change_definition_user = "lastChangedDefinitionUser",
                  schemer = "schemer",
                  scheme_type = "schemeType",
                  last_change_scheme = "lastChangedScheme",
                  last_change_scheme_user = "lastChangedSchemeUser",
                  last_change_metadata = "lastChangedMetadata",
                  last_change_metadata_user = "lastChangedMetadataUser"
                )))

              if (metadata) {
                # TODO: Settings woanders einbinden und hier übergeben?
                unit <- prepare_metadata(unit, resp_metadata, workspace)
              }

              if (unit_definition) {
                run_req_definition <- function() {
                  base_req(method = "GET",
                           endpoint = c("workspace", ws_id, unit_id, "definition")) %>%
                    httr2::req_perform() %>%
                    httr2::resp_body_json()
                }

                resp_definition <-
                  run_safe(run_req_definition,
                           error_message = "Unit definition could not be retrieved.")

                if (!is.null(resp_definition)) {
                  unit <- prepare_definition(unit, resp_definition)
                }
              }

              if (coding_scheme) {
                run_req_scheme <- function() {
                  base_req(method = "GET",
                           endpoint = c("workspace", ws_id, unit_id, "scheme")) %>%
                    httr2::req_perform() %>%
                    httr2::resp_body_json()
                }

                resp_scheme <-
                  run_safe(run_req_scheme,
                           error_message = "Unit scheme could not be retrieved.")

                if (!is.null(resp_scheme)) {
                  unit <- prepare_scheme(unit, resp_scheme)
                }
              }

              unit
            } else {
              tibble::tibble()
            }
          })
