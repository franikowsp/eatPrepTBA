prepareUnitXml <- function(xml) {
  base_variables <-
    xml %>%
    dplyr::select(BaseVariables) %>%
    dplyr::mutate(
      BaseVariables = purrr::map(BaseVariables, function(x) x %>%
                                   tibble::enframe(name = NULL))) %>%
    tidyr::unnest(BaseVariables) %>%
    dplyr::mutate(
      value = purrr::map(value, function(x) {
        # TODO: Hotfix for textarea
        if (length(x$value) > 0) {
          x$Values %>%
            tibble::enframe(name = NULL) %>%
            dplyr::mutate(
              value = purrr::map(value, function(x) x %>%
                                   tibble::enframe() %>%
                                   tidyr::pivot_wider())
            ) %>%
            tidyr::unnest(value) %>%
            tidyr::unnest(c(label, value)) %>%
            tidyr::unnest(c(label, value))
        } else {
          tibble::tibble()
        }
      })
    ) %>%
    tidyr::unnest(value)

  # TODO: Hotfix for textarea
  if ("label" %in% names(base_variables)) {
    base_variables <-
      dplyr::rename(
        option_label = label
      )
  }

  metadata <-
    xml$Metadata[[1]] %>%
    tibble::enframe() %>%
    tidyr::unnest(value) %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider() %>%
    dplyr::select(
      -Id,
      unit_label = Label
    )

  # TODO: hotfix for textarea
  if (nrow(base_variables) > 0) {
    dplyr::cross_join(
      metadata,
      base_variables
    )
  } else {
    metadata
  }
}
