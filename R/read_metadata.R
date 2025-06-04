#' @keywords internal
read_metadata <- function(units) {
  unit_keys <- units$unit_key

  if (length(unit_keys) > 0) {
    units %>%
      dplyr::mutate(
        unit_metadata = purrr::map(unit_metadata,
                                   prepare_metadata,
                                   .progress = list(
                                     type ="custom",
                                     extra = list(
                                       unit_keys = pad_ids(unit_keys)
                                     ),
                                     format = "Preparing metadata for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                     format_done = "Prepared {cli::pb_total} unit metadata in {cli::pb_elapsed}.",
                                     clear = FALSE
                                   ))) %>%
      tidyr::unnest(unit_metadata)
  } else {
    units %>%
      dplyr::select(-unit_metadata)
  }
}
