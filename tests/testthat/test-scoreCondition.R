test_that("translates the MATCH method", {
  expect_equal(scoreCondition("MATCH", 100), "value == 100")
  # TODO: This does not work, yet! [must be "test"]
  # expect_equal(scoreCondition("MATCH", "test"), "value == test")
})

test_that("translates the MATCH_REGEX method", {
  expect_equal(scoreCondition("MATCH_REGEX", "^[Tt]est}$"), "stringr::str_detect(value, \"^[Tt]est}$\")")
})

test_that("translates the NUMERIC_MIN method", {
  expect_equal(scoreCondition("NUMERIC_MIN", 100), "value >= 100")
})

test_that("translates the NUMERIC_MAX method", {
  expect_equal(scoreCondition("NUMERIC_MAX", 100), "value <= 100")
})

test_that("translates the NUMERIC_LESS_THEN method", {
  expect_equal(scoreCondition("NUMERIC_LESS_THEN", 100), "value < 100")
})

test_that("translates the NUMERIC_MORE_THEN method", {
  expect_equal(scoreCondition("NUMERIC_MORE_THEN", 100), "value > 100")
})

test_that("translates the NUMERIC_RANGE method", {
  expect_equal(scoreCondition("NUMERIC_RANGE", c(0, 100)), "(0 < value) & (value <= 100)")
})

test_that("translates the IS_EMPTY method", {
  expect_equal(scoreCondition("IS_EMPTY"), "length(value) == 0")
})

test_that("translates the ELSE method", {
  expect_equal(scoreCondition("ELSE"), "TRUE")
})

test_that("translates an undefined method", {
  expect_equal(scoreCondition("undefined"), "FALSE")
})
