# MATCH methods -----------------------------------------------------------
codes <-
  tibble::tibble(
    code_id = c(1),
    code_label = c(1),
    score = c(1),
    method = c(""),
    code_value = c(1),
    code_manual_instruction = ""
  )

test_that("translates the MATCH method", {
  codes_match <- codes
  codes_match$method <- "MATCH"

  score_match <- scoreFunction(codes_match)

  expect_equal(score_match(value = 1),
               dplyr::slice(codes_match, 1))

  expect_equal(score_match(value = 2),
               dplyr::slice(codes_match, 0))
})

test_that("translates the MATCH method for lists without order constraints", {
  codes_match <- codes
  codes_match$method <- "MATCH"
  codes_match$code_value <- "1#2#3#4"

  score_match <- scoreFunction(codes_match)

  expect_equal(score_match(value = c(1,2,3,4)),
               dplyr::slice(codes_match, 1))

  expect_equal(score_match(value = c(1,2,3)),
               dplyr::slice(codes_match, 0))

  expect_equal(score_match(value = c(1)),
               dplyr::slice(codes_match, 0))

  expect_equal(score_match(value = c(4, 3, 2, 1)),
               dplyr::slice(codes_match, 0))
})


test_that("translates the MATCH method for lists with order constraints", {
  codes_match <- codes
  codes_match$method <- "MATCH"
  codes_match$code_value <- "!1#2#3#4"

  score_match <- scoreFunction(codes_match)

  expect_equal(score_match(value = c(1,2,3,4)),
               dplyr::slice(codes_match, 1))

  expect_equal(score_match(value = c(1,2,4,3)),
               dplyr::slice(codes_match, 0))

  expect_equal(score_match(value = c(1,2,3)),
               dplyr::slice(codes_match, 0))

  expect_equal(score_match(value = c(1)),
               dplyr::slice(codes_match, 0))

  expect_equal(score_match(value = c(4, 3, 2, 1)),
               dplyr::slice(codes_match, 0))
})

test_that("translates the MATCH_REGEX method", {
  codes_match_regex <- codes
  codes_match_regex$method <- "MATCH_REGEX"
  codes_match_regex$code_value <- "^[Ee]nten?$"

  score_match_regex <- scoreFunction(codes_match_regex)

  expect_equal(score_match_regex(value = "Ente"),
               dplyr::slice(codes_match_regex, 1))

  expect_equal(score_match_regex(value = "Enten"),
               dplyr::slice(codes_match_regex, 1))

  expect_equal(score_match_regex(value = "ente"),
               dplyr::slice(codes_match_regex, 1))

  expect_equal(score_match_regex(value = "Rente"),
               dplyr::slice(codes_match_regex, 0))

})

# NUMERIC methods ---------------------------------------------------------
codes_numeric <- codes
codes_numeric$code_value <- c(100)

test_that("translates the NUMERIC_MIN method", {
  codes_numeric_min <- codes_numeric
  codes_numeric_min$method <- "NUMERIC_MIN"

  score_numeric_min <- scoreFunction(codes_numeric_min)

  expect_equal(score_numeric_min(value = 101),
               dplyr::slice(codes_numeric_min, 1))

  expect_equal(score_numeric_min(value = 100),
               dplyr::slice(codes_numeric_min, 1))

  expect_equal(score_numeric_min(value = 99),
               dplyr::slice(codes_numeric_min, 0))
})


test_that("translates the NUMERIC_MIN method", {
  codes_numeric_max <- codes_numeric
  codes_numeric_max$method <- "NUMERIC_MAX"

  score_numeric_max <- scoreFunction(codes_numeric_max)

  expect_equal(score_numeric_max(value = c(99)),
               dplyr::slice(codes_numeric_max, 1))

  expect_equal(score_numeric_max(value = 100),
               dplyr::slice(codes_numeric_max, 1))

  expect_equal(score_numeric_max(value = 101),
               dplyr::slice(codes_numeric_max, 0))
})

test_that("translates the NUMERIC_LESS_THEN method", {
  codes_numeric_less_then <- codes_numeric
  codes_numeric_less_then$method <- "NUMERIC_LESS_THEN"

  score_numeric_less_then <- scoreFunction(codes_numeric_less_then)

  expect_equal(score_numeric_less_then(value = c(99)),
               dplyr::slice(codes_numeric_less_then, 1))

  expect_equal(score_numeric_less_then(value = 100),
               dplyr::slice(codes_numeric_less_then, 0))

  expect_equal(score_numeric_less_then(value = 101),
               dplyr::slice(codes_numeric_less_then, 0))
})

test_that("translates the NUMERIC_MORE_THEN method", {
  codes_numeric_more_then <- codes_numeric
  codes_numeric_more_then$method <- "NUMERIC_MORE_THEN"

  score_numeric_more_then <- scoreFunction(codes_numeric_more_then)

  expect_equal(score_numeric_more_then(value = c(101)),
               dplyr::slice(codes_numeric_more_then, 1))

  expect_equal(score_numeric_more_then(value = 100),
               dplyr::slice(codes_numeric_more_then, 0))

  expect_equal(score_numeric_more_then(value = 99),
               dplyr::slice(codes_numeric_more_then, 0))
})

test_that("translates the NUMERIC_RANGE method", {
  codes_numeric_range <- codes_numeric
  codes_numeric_range$method <- "NUMERIC_RANGE"
  codes_numeric_range$code_value <- list(c(100, 102))

  score_numeric_range <- scoreFunction(codes_numeric_range)

  expect_equal(score_numeric_range(value = 100),
               dplyr::slice(codes_numeric_range, 0))

  expect_equal(score_numeric_range(value = 101),
               dplyr::slice(codes_numeric_range, 1))

  expect_equal(score_numeric_range(value = 102),
               dplyr::slice(codes_numeric_range, 1))

  expect_equal(score_numeric_range(value = 103),
               dplyr::slice(codes_numeric_range, 0))
})

# Remaining methods -------------------------------------------------------
# test_that("translates the IS_EMPTY method", {
#   # TODO: Check this after seeing cases
#   codes_is_empty <- codes
#   codes_is_empty$method <- "IS_EMPTY"
#   codes_is_empty$code_value <- NA
#
#   score_is_empty <- scoreFunction(codes_is_empty)
#
#   expect_equal(score_is_empty(value = NA),
#                dplyr::slice(codes_is_empty, 1))
#
#   expect_equal(score_is_empty(value = 1),
#                dplyr::slice(codes_is_empty, 0))
#
# })

test_that("translates the ELSE method", {
  codes_else <-
    tibble::tibble(
      code_id = c(1, 2),
      code_label = c(1, 2),
      score = c(1, 2),
      method = c("MATCH", "ELSE"),
      code_value = c(1, NA),
      code_manual_instruction = c("", "")
    )

  score_else <- scoreFunction(codes_else)

  expect_equal(score_else(value = 1),
               dplyr::slice(codes_else, 1))

  expect_equal(score_else(value = 2),
               dplyr::slice(codes_else, 2))

  expect_equal(score_else(value = NA),
               dplyr::slice(codes_else, 2))

})
