#' @keywords internal
pad_ids <- function(unit_keys) {
  max_length <- unit_keys %>% stringr::str_length() %>% max()

  unit_keys %>% stringr::str_pad(width = max_length, side = "right")
}
