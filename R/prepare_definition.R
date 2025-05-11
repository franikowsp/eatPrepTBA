prepare_definition <- function(resp_definition) {
  unit_definition <-
    resp_definition$definition %>%
    jsonlite::parse_json()

  # TODO: If this comes directly from the IQB Studio, this
  # will no longer be necessary
  # Marking panels should also be added to connect these with the variables for
  # replay mode
  variable_pages <-
    unit_definition$pages %>%
    purrr::imap(function(page, i) {
      variable_page_always_visible <-
        get_deepest_elements(page, label = "alwaysVisible") %>%
        purrr::list_simplify()

      variable_ref <-
        get_deepest_elements(page, label = c("id")) %>%
        purrr::list_simplify()

      # variable_marker <-
      #   get_deepest_elements(page, label = "markingPanels") %>%
      #   purrr::list_flatten()

      list(
        variable_page = i,
        variable_ref = variable_ref,
        variable_page_always_visible = variable_page_always_visible#,
        # variable_marker = variable_marker
      )
    }) %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    tidyr::unnest(variable_ref) %>%
    dplyr::group_by(variable_page_always_visible) %>%
    dplyr::mutate(
      # TODO: Are these pages in order?!
      variable_page = variable_page - min(variable_page)
    ) %>%
    dplyr::ungroup()

  tibble::tibble(
    unit_definition = list(unit_definition),
    variable_pages = list(variable_pages)
  )
}
