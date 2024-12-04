customize_texts <- function(...) {
  args <- rlang::list2(...)

  args %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("CustomText")
}
