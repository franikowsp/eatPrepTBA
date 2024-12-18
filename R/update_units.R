#' #' Update multiple units with resources
#' #'
#' #' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' #' @param units Tibble. Contains the units retrieved from a workspace in the IQB Studio that should be updated.
#' #' @param add_units Logical. Should only the available units in the provides `units` argument be updated (`FALSE`) or should additional units potentially be added (`TRUE`)? Defaults to `TRUE`.
#' #' @param verbose Logical. Should the function return additional messages. Defaults to `FALSE`.
#' #'
#' #' @description
#' #' This function returns the unit information for multiple units by repeatedly calling [get_unit()]. Does currently only update available columns
#' #'
#' #' @return A tibble.
#' #' @export
#' #'
#' #' @aliases
#' #' update_units,WorkspaceStudio-method
#' setGeneric("update_units", function(workspace,
#'                                     units,
#'                                     add_units = TRUE,
#'                                     verbose = FALSE) {
#'   standardGeneric("update_units")
#' })
#'
#' #' @describeIn update_units Update multiple unit information and coding schemes in a given Studio workspace
#' setMethod("update_units",
#'           signature = signature(workspace = "WorkspaceStudio"),
#'           function(workspace,
#'                    units,
#'                    add_units = TRUE,
#'                    verbose = FALSE) {
#'             unit_ids <- NULL
#'
#'             if (!add_units) {
#'               unit_ids <- unique(units$unit_id)
#'             }
#'
#'             units_update <- get_units(workspace,
#'                                       unit_ids = unit_ids,
#'                                       metadata = FALSE,
#'                                       unit_definition = FALSE,
#'                                       coding_scheme = FALSE)
#'
#'             units_ts <-
#'               get_timestamps(units)
#'
#'             units_update_ts <-
#'               get_timestamps(units_update)
#'
#'             units_update_operation <-
#'               units_ts %>%
#'               dplyr::left_join(units_update_ts %>% dplyr::rename(new_change = last_change)) %>%
#'               dplyr::mutate(
#'                 update = new_change > last_change,
#'                 object = dplyr::case_when(
#'                   object == "definition" ~ "unit_definition",
#'                   object == "scheme" ~ "coding_scheme",
#'                   .default = object
#'                 )
#'               ) %>%
#'               dplyr::select(unit_key, unit_id, object, update) %>%
#'               tidyr::nest(
#'                 args = c(object, update)
#'               ) %>%
#'               dplyr::mutate(
#'                 args = purrr::map(args, tibble::deframe)
#'               )
#'
#'             # purrr::exec(.fn = get_unit, !!! c(workspace = workspace,
#'             #                                   units_update_operation$unit_id[[1]],
#'             #                                   units_update_operation$args[[1]]))
#'             #
#'             # dplyr::rows
#'             #
#'             # ... und wie soll es weitergehen? Eigentlich müssten ja nur die betreffenden Einträge ausgetauscht werden
#'           })
#'
#' get_timestamps <- function(units) {
#'   units %>%
#'     dplyr::select(dplyr::any_of(
#'       c("unit_id", "unit_key",
#'         "last_change_definition", "last_change_scheme", "last_change_metadata")
#'     )) %>%
#'     tidyr::pivot_longer(dplyr::any_of(c("last_change_definition", "last_change_scheme", "last_change_metadata")),
#'                         names_to = "object",
#'                         names_prefix = "last_change_",
#'                         values_to = "last_change") %>%
#'     dplyr::mutate(
#'       last_change = as.POSIXct(last_change, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "CET")
#'     )
#' }
