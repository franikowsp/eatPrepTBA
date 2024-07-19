get_deepest_elements <- function(x, label) {
  if (is.list(x)) {
    if (label %in% names(x)) {
      elements <- list(x[[label]])
    } else {
      elements <- list()
    }

    elements <-
      c(elements,
        purrr::flatten(purrr::map(x, function(node) {
          node %>%
            get_deepest_elements(label = label) %>%
            unname()
        })))
  } else {
    elements <- NULL
  }
}
