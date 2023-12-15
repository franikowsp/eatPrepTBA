scoreCondition <- function(method, code_value, value_transformations = NULL) {
  # TODO: add test to check if ! is.na(code_value) ?
  value_placeholder <- "value"
  if (length(value_transformations) > 0) {
    if ("TO_UPPER" %in% value_transformations) {
      value_placeholder <- stringr::str_glue("stringr::str_to_upper({value_placeholder})")
    }

    if ("REMOVE_WHITE_SPACES" %in% value_transformations) {
      value_placeholder <- stringr::str_glue("stringr::str_remove_all({value_placeholder}, \" \")")
    }
  }

  check_order <- stringr::str_detect(code_value, "^!")

  if (stringr::str_detect(code_value, "#")) {
    code_value <-
      stringr::str_remove(code_value, "^!") %>%
      stringr::str_split_1(pattern = "#")

    code_value <- stringr::str_c(code_value, collapse = ",")

    if (check_order) {
      # TODO: Check how many values are in the correct order
      # TODO: Check if character conversion is necessary
      method_match <- "length(value) > 0 &&
      identical(as.character(c({code_value})), {value_placeholder})"
    } else {
      # all(code_values %in% values) & all(values %in% code_values)
      # TODO: Check if character conversion is necessary
      method_match <- "length(value) > 0 &&
      setequal(as.character(c({code_value})), {value_placeholder})"
    }
  } else {
    method_match <- "length(value) > 0 && {value_placeholder} == c({code_value})"
  }

  string <-
    switch(
      method,
      # TODO: Does not work on strings - should only be used for numbers
      # Proposal: NUMERIC_MATCH method?
      "MATCH" = method_match,
      # Should be used for strings
      "MATCH_REGEX" = "length(value) > 0 && stringr::str_detect({value_placeholder}, \"{code_value}\")",
      "NUMERIC_MIN" = "length(value) > 0 && {value_placeholder} >= {code_value}",
      "NUMERIC_MAX" = "length(value) > 0 && {value_placeholder} <= {code_value}",
      "NUMERIC_LESS_THEN" = "length(value) > 0 &&& {value_placeholder} < {code_value}",
      "NUMERIC_MORE_THEN" = "length(value) > 0 &&& {value_placeholder} > {code_value}",
      "NUMERIC_RANGE" = "length(value) > 0 && ({code_value[1]} < {value_placeholder}) & ({value_placeholder} <= {code_value[2]})",
      "IS_EMPTY" = "length(value) == 0",
      "ELSE" = "TRUE",
      # Default
      "FALSE")

  stringr::str_glue(string)
}
