deriveFunction <- function(variable_id,
                           source_type,
                           derive_sources,
                           derive_source_type) {
  # TODO: precedence of variable preparations
  # derive_sources <- purrr::map(derive_sources,
  #                              function(x) purrr::reduce(x, c))
  derive_sources <- unlist(derive_sources)
  derive_sources_vector <-
    stringr::str_glue("\"{derive_sources}\"") %>%
    stringr::str_c(collapse = ", ")

  deriveVariable <-
    switch(
      derive_source_type,
      "SCORE" = "score",
      "CODE" = "code_id",
      "VALUE" = "code_value"
    )

  deriveOperation <-
    switch(
      source_type,
      "DERIVE_SUM" = stringr::str_glue("sum({deriveVariable})"),
      "DERIVE_CONCAT" = stringr::str_glue("paste0({deriveVariable}, collapse = \", \")")
    )

  statement <-
    stringr::str_glue("data %>%
                        dplyr::filter(
                          variable_id %in% c({derive_sources_vector})
                        ) %>%
                        dplyr::summarise(
                          value = as.character({deriveOperation})
                        )")

  function(data) {
    derive_table <-
      eval(
        parse(text = statement)
      )

    derive_table %>%
      dplyr::mutate(
        variable_id = variable_id,
        source_type = source_type,
        derive_sources = list(derive_sources),
        derive_source_type = derive_source_type
      )
  }
}
