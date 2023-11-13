# Prepare tests with global variable --------------------------------------
# originalTestMode <- getOption("eatPrepTBA.test_mode")

options("eatPrepTBA.test_mode" = TRUE)

cli::test_that_cli("login works as inteded", {
  expect_snapshot(createLogin())
})

cli::test_that_cli("login fails with wrong or omitted name and/or password", {
  expect_snapshot(createLogin(name = "eatPrepTBA", password = ""))
  expect_snapshot(createLogin(name = "", password = "eatPrepTBA"))
  expect_snapshot(createLogin(name = "", password = ""))
})

options("eatPrepTBA.test_mode" = originalTestMode)
