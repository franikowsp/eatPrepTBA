scoreFunction <- function(codes) {
  # TODO: add argument for value_transformations
  checks <-
    purrr::map2(codes$method, codes$code_value, scoreCondition)

  # Generates the if-else tree of the conditions necessary
  statement <-
    checks %>%
    purrr::imap(function(x, i) stringr::str_glue("if ({x}) {{ {i} }}")) %>%
    purrr::reduce(function(old, new) stringr::str_c(c(old, new), collapse = " else "))

  function(value) {
    val <-
      eval(
        parse(text = statement)
      )

    codes %>%
      dplyr::slice(val)
  }
}
