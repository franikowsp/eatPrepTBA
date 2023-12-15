scoreFunction <- function(codes, value_transformations = NULL, verbose = FALSE) {
  checks <-
    purrr::pmap(list(codes$method, codes$code_value),
                function(method, code_value) {
                  scoreCondition(method, code_value, value_transformations)
                })

  # Generates the if-else tree of the conditions necessary
  statement <-
    checks %>%
    purrr::imap(function(x, i) stringr::str_glue("if ({x}) {{ \n\t {i} \n }}")) %>%
    purrr::reduce(function(old, new) stringr::str_c(c(old, new), collapse = " else "))

  if (verbose) {
    cli::cli_verbatim(statement)
  }

  function(value) {
    val <-
      eval(
        parse(text = statement)
      )

    codes %>%
      dplyr::slice(val)
  }
}
