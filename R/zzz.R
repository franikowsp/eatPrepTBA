.onAttach <- function(libname, pkgname) {
  cli_setting()

  version <- utils::packageVersion(pkgname)

  cli::cli_alert_info("{.pkg {pkgname}} v{version}",
                      class = "packageStartupMessage")
  # rlang::inform(message = msg, class = "packageStartupMessage")
}
