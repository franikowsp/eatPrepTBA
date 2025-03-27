prepare_definition <- function(resp_definition) {
  unit_definition <-
    resp_definition$definition %>%
    jsonlite::parse_json()

  # TODO: If this comes directly from the IQB Studio, this
  # will no longer be necessary
  variable_pages <-
    unit_definition$pages %>%
    purrr::keep(function(x) !x$alwaysVisible) %>%
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

  tibble::tibble(
      unit_definition = list(unit_definition),
      variable_pages = list(variable_pages)
    )
}
