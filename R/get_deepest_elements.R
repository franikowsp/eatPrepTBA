get_deepest_elements <- function(x, label, no_parent = NULL) {
  if (!is.list(x)) {
    return(NULL)
  }

  elements <- list()
  if (label %in% names(x)) {
    elements <- list(x[[label]])
  }

  c(elements,
    purrr::flatten(purrr::imap(x, function(node, node_name) {
      if (! node_name %in% no_parent) {
        node %>%
          get_deepest_elements(label = label, no_parent = no_parent) %>%
          unname()
      } else {
        NULL
      }
    })))
}
