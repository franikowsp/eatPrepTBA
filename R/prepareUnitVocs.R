prepareUnitVocs <- function(vocs) {
  vocs_table <-
    vocs %>%
    dplyr::mutate(
      value = purrr::map(value, function(x) x %>%
                           tibble::enframe() %>%
                           tidyr::pivot_wider() %>%
                           tidyr::unnest(c(id,
                                           sourceType,
                                           status,
                                           deriveSourceType,
                                           manualInstruction,
                                           label))
      )) %>%
    tidyr::unnest(value) %>%
    dplyr::filter(!stringr::str_detect(id, "^_")) %>%
    dplyr::mutate(codes = purrr::map(codes, function(x) {
      x %>%
        purrr::map(function(x) tibble::enframe(x) %>% tidyr::pivot_wider()) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        tidyr::unnest(cols = c(id, label, score, rules, manualInstruction)) %>%
        dplyr::mutate(
          rules = purrr::map(rules, tibble::enframe),
        ) %>%
        tidyr::unnest(rules) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        tidyr::unnest(cols = c(method, parameters), keep_empty = TRUE) %>%
        dplyr::mutate(
          parameters = purrr::map(parameters,
                                  function(x) stringr::str_c(x, collapse = ","))
        ) %>%
        tidyr::unnest(parameters) %>%
        dplyr::select(
          code_id = id,
          code_label = label,
          score = score,
          method = method,
          code_value = parameters,
          code_manual_instruction = manualInstruction
        )
    })) %>%
    dplyr::select(
      -name,
      variable_id = id,
      variable_label = label,
      source_type = sourceType,
      codes = codes,
      status = status,
      derive_source_type = deriveSourceType,
      derive_sources = deriveSources,
      variable_manual_instruction = manualInstruction,
      value_transformations = valueTransformations
    )
}
