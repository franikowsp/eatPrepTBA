customTexts <- function(...) {
  list(
    ...
  ) %>%
    purrr::set_names("CustomText")
}
