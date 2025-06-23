prepare_definition <- function(resp_definition) {
  # definition_prep <-
  variable_pages <-
    resp_definition %>%
    eatAutoCode::extract_variable_location() %>%
    tibble::as_tibble() %>%
    tidyr::unnest(variable_path, keep_empty = TRUE) %>%
    dplyr::select(
      dplyr::any_of(c(
        "variable_ref" = "variable_ref",
        "variable_page_always_visible" = "variable_page_always_visible",
        "variable_dependencies" = "variable_dependencies",

        "variable_page" = "pages",
        "variable_section" = "sections",
        "variable_element" = "elements",
        "variable_content" = "content"
      ))
    ) %>%
    dplyr::mutate(
      variable_dependencies = purrr::map2(variable_page_always_visible,
                                          variable_dependencies,
                                          function(page_always_visible, dependencies) {
                                            if (page_always_visible & length(dependencies) != 0) {
                                              dependencies
                                            } else {
                                              tibble::tibble(
                                                variable_dependency_ref = NA_character_,
                                                variable_dependency_path = tibble::tibble(),
                                                variable_dependency_page_always_visible = NA)
                                            }
                                          })
    ) %>%
    tidyr::unnest(variable_dependencies, keep_empty = TRUE) %>%
    # tidyr::unnest(variable_dependencies, keep_empty = TRUE) %>%
    tidyr::unnest(dplyr::any_of("variable_dependency_path"), keep_empty = TRUE) %>%
    dplyr::filter(is.na(variable_dependency_page_always_visible) |
                    !variable_dependency_page_always_visible) %>%
    dplyr::rename(
      dplyr::any_of(c(
        "variable_ref" = "variable_ref",
        "variable_page_always_visible" = "variable_page_always_visible",
        "variable_dependencies" = "variable_dependencies",

        "variable_dependency_page" = "pages",
        "variable_dependency_section" = "sections",
        "variable_dependency_content" = "content",
        "variable_dependency_element" = "element"
      ))
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("variable_ref",
                                                  "variable_page",
                                                  "variable_section",
                                                  "variable_content",
                                                  "variable_element",
                                                  "variable_page_always_visible")))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::any_of(c("variable_dependency_page",
                                    "variable_dependency_page",
                                    "variable_dependency_section",
                                    "variable_dependency_content",
                                    "variable_dependency_element")),
                    min),
      .groups = "drop"
    )

  if (tibble::has_name(variable_pages, "variable_dependency_page")) {
    variable_pages <-
      variable_pages %>%
      dplyr::mutate(
        variable_page = ifelse(variable_page_always_visible & !is.na(variable_dependency_page),
                               variable_dependency_page,
                               variable_page)
      ) %>%
      dplyr::select(-dplyr::matches("^variable_dependency"))
  }

  # unit_definition <-
  #   resp_definition$definition %>%
  #   jsonlite::parse_json()

  # TODO: If this comes directly from the IQB Studio, this
  # will no longer be necessary
  # Marking panels should also be added to connect these with the variables for
  # replay mode
  # variable_pages <-
  #   unit_definition$pages %>%
  #   purrr::imap(function(page, i) {
  #     variable_page_always_visible <-
  #       get_deepest_elements(page, label = "alwaysVisible") %>%
  #       purrr::list_simplify()
  #
  #     variable_ref <-
  #       get_deepest_elements(page, label = "id", no_parent = "visibilityRules") %>%
  #       purrr::list_simplify()
  #
  #     # variable_marker <-
  #     #   get_deepest_elements(page, label = "markingPanels") %>%
  #     #   purrr::list_flatten()
  #
  #     list(
  #       variable_page = i,
  #       variable_ref = variable_ref,
  #       variable_page_always_visible = variable_page_always_visible#,
  #       # variable_marker = variable_marker
  #     )
  #   }) %>%
  #   purrr::list_transpose() %>%
  #   tibble::as_tibble() %>%
  #   tidyr::unnest(variable_ref) %>%
  #   dplyr::group_by(variable_page_always_visible) %>%
  #   dplyr::mutate(
  #     # TODO: Are these pages in order?!
  #     variable_page = variable_page - min(variable_page)
  #   ) %>%
  #   dplyr::ungroup()

  tibble::tibble(
    unit_definition = resp_definition,
    variable_pages = list(variable_pages)
  )
}
