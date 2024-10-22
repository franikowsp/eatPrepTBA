customize_texts <- function(...) {
  (...) %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("CustomText")
}
