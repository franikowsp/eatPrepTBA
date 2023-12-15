deriveFunction <- function(variable_id,
                           source_type,
                           derive_sources,
                           derive_source_type,
                           verbose = FALSE) {
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
      "VALUE" = "value"
    )

  deriveOperation <-
    switch(
      source_type,
      "DERIVE_SUM" = stringr::str_glue("sum(as.numeric({deriveVariable}))"),
      "DERIVE_CONCAT" = stringr::str_glue("paste0(unlist({deriveVariable}), collapse = \"\")")
    )

  statement <-
    stringr::str_glue("data %>%
                       \t dplyr::filter(
                       \t\t variable_id %in% c({derive_sources_vector})
                       \t ) %>%
                       \t dplyr::summarise(
                       \t\t value = as.character({deriveOperation})
                       \t )")

  if (verbose) {
    cli::cli_verbatim(statement)
  }

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
