scoreCondition <- function(method, code_value) {
  # TODO: add test to check if ! is.na(code_value) ?
  # TODO: add routines for Uppercase and Delete space

  check_order <- stringr::str_detect(code_value, "^!")

  code_value <-
    stringr::str_remove(code_value, "^!") %>%
    stringr::str_split_1(pattern = "#")

  if (length(code_value) > 1) {
    code_value <- stringr::str_c(code_value, collapse = ",")

    if (check_order) {
      # TODO: Check how many values are in the correct order
      method_match <- "! any(is.na(value)) &
      identical(c({code_value}), value)"
    } else {
      # all(code_values %in% values) & all(values %in% code_values)
      method_match <- "! any(is.na(value)) &
      setequal(c({code_value}), value)"
    }
  } else {
    method_match <- "! is.na(value) & value == {code_value}"
  }

  string <-
    switch(
      method,
      # TODO: Does not work on strings - should only be used for numbers
      # Proposal: NUMERIC_MATCH method?
      "MATCH" = method_match,
      # Should be used for strings
      "MATCH_REGEX" = "! is.na(value) & stringr::str_detect(value, \"{code_value}\")",
      "NUMERIC_MIN" = "! is.na(value) & value >= {code_value}",
      "NUMERIC_MAX" = "! is.na(value) & value <= {code_value}",
      "NUMERIC_LESS_THEN" = "! is.na(value) & value < {code_value}",
      "NUMERIC_MORE_THEN" = "! is.na(value) & value > {code_value}",
      "NUMERIC_RANGE" = "! is.na(value) & ({code_value[1]} < value) & (value <= {code_value[2]})",
      "IS_EMPTY" = "is.na(value)",
      "ELSE" = "TRUE",
      # Default
      "FALSE")

  stringr::str_glue(string)
}
