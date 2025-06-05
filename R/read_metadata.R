#' @keywords internal
read_metadata <- function(units) {
  unit_keys <- units$unit_key

  if (length(unit_keys) > 0) {
    ws_ids <- units$ws_id
    n_ws_ids <- length(unique(ws_ids))

    units %>%
      dplyr::mutate(
        unit_metadata = purrr::map(unit_metadata,
                                   prepare_metadata,
                                   .progress = list(
                                     type ="custom",
                                     extra = list(
                                       unit_keys = pad_ids(unit_keys)
                                       ws_ids = ws_ids,
                                       n_ws_ids = n_ws_ids
                                     ),
                                     format = "Preparing metadata for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                     format_done = "Prepared metadata of {cli::pb_total} {.unit-label unit{?s}} of {cli::pb_extra$n_ws_ids} {.ws-label workspace{?s}} metadata in {cli::pb_elapsed}.",
                                     format_failed = "Failed at preparing metadata for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}}",
                                     clear = FALSE
                                   ))) %>%
      tidyr::unnest(unit_metadata)
  } else {
    units %>%
      dplyr::select(-unit_metadata)
  }
}
