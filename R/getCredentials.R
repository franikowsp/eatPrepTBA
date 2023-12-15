getCredentials <- function(domain, dialog, keyring, changeKey) {
  isRStudio <- Sys.getenv("RSTUDIO") == "1"
  test_mode <- getOption("eatPrepTBA.test_mode")

  name_prompt <- stringr::str_glue("Enter your username for {domain}: ")
  password_prompt <- stringr::str_glue("Enter your password for {domain}: ")


  if (keyring) {
    name <- keyring::key_list(service = domain)[["username"]]
    hasKey <- length(name) != 0

    if (! hasKey || changeKey) {
      if (changeKey) {
        keyring::key_delete(service = domain, username = name)
      }

      if (isRStudio) {
        name <- rstudioapi::askForPassword(name_prompt)
      } else {
        name <- readline(name_prompt)
      }

      keyring::key_set(service = domain, username = name)
    }

    credentials <- list(
      name = name,
      password = URLencode(keyring::key_get(service = domain, username = name), reserved = TRUE)
    )

  } else {
    if (is.null(test_mode) || ! test_mode) {
      if (isRStudio & dialog) {
        name <- rstudioapi::askForPassword(name_prompt)
        password <- rstudioapi::askForPassword(password_prompt)
      } else {
        name <- readline(prompt = name_prompt)
        password <- readline(prompt = password_prompt)
      }

      credentials <- list(
        name = name,
        password = URLencode(password, reserved = TRUE)
      )
    } else {
      # Routine for testing purposes only
      credentials <- list(...)

      if (is.null(credentials) || is.null(credentials$name) || is.null(credentials$password)) {
        credentials <- list(
          name = "eatPrepTBA",
          password = URLencode("eatPrepTBA", reserved = TRUE)
        )
      }
    }
  }

  return(credentials)
}
