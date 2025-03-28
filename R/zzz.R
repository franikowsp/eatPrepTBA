.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  cli::cli_alert_info("{.pkg {pkgname}} v{version}",
                      class = "packageStartupMessage")
  # rlang::inform(message = msg, class = "packageStartupMessage")
}
