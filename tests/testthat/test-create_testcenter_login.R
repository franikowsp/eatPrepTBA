# # Prepare tests with global variable --------------------------------------
# originalTestMode <- getOption("eatPrepTBA.test_mode")
#
# options("eatPrepTBA.test_mode" = TRUE)
#
# cli::test_that_cli("login works as inteded", {
#   expect_snapshot(createTestcenterLogin())
# })
#
# cli::test_that_cli("login fails with wrong or omitted name and/or password", {
#   expect_snapshot(createTestcenterLogin(name = "eatPrepTBA", password = ""))
#   expect_snapshot(createTestcenterLogin(name = "", password = "eatPrepTBA"))
#   expect_snapshot(createTestcenterLogin(name = "", password = ""))
# })
#
# options("eatPrepTBA.test_mode" = originalTestMode)
