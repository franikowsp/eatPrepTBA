.onAttach <- function(libname, pkgname) {
  cli::cli_alert_info("{.pkg {pkgname}} v{packageVersion(pkgname)}")
  # rlang::inform(message = msg, class = "packageStartupMessage")
}

