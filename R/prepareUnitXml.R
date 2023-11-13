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
      })
    ) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(
      option_label = label
    )

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

  dplyr::cross_join(
    metadata,
    base_variables
  )
}
