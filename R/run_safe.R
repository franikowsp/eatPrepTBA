run_safe <- function(req = req,
                     error_message = "Something went wrong with the API request",
                     default = NULL) {
  cli_setting()

  tryCatch(
    error = function(cnd) {
      cli::cli_alert_danger(error_message,
                            wrap = TRUE)

      cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")

      if (!is.null(default)) {
        # Default return
        return(default)
      }
    },
    req()
  )
}
