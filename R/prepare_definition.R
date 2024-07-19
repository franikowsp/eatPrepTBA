prepare_definition <- function(unit, resp_definition) {
  unit_variables <-
    resp_definition$variables %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    tidyr::unnest(values)

  if (exists("values", unit_variables)) {
    unit_variables <-
      unit_variables %>%
      dplyr::mutate(
        values = purrr::map(values, function(x) {
          if (length(x) == 0) {
            tibble::tibble(value = NA, label = NA)
          } else {
            tibble::as_tibble(x)
          }
        })
      ) %>%
      tidyr::unnest(values) %>%
      dplyr::rename(
        any_of(c(
          variable_id = "id",
          value_label = "label",
          values_complete = "valuesComplete",
          values_position_labels = "value_position_labels"
        ))
      )
  } else {
    unit_variables <-
      unit_variables %>%
      dplyr::mutate(
        varible_label = NA_character_,
        value = NA_character_
      ) %>%
      dplyr::rename(variable_id = id)
  }

  unit_definition <-
    resp_definition$definition %>%
    jsonlite::parse_json()

  # TODO: If this comes directly from the IQB Studio, this
  # will no longer be necessary
  variable_pages <-
    unit_definition$pages %>%
    purrr::map(function(page) {
      get_deepest_elements(page, label = "id") %>%
        purrr::list_simplify()
    }) %>%
    tibble::enframe(name = "page",
                    value = "variable_id") %>%
    tidyr::unnest(variable_id) %>%
    dplyr::mutate(
      page = page - 1
    )

  unit %>%
    dplyr::mutate(
      unit_variables = list(unit_variables),
      unit_definition = list(unit_definition),
      variable_pages = list(variable_pages)
    )
}
