#' Generate a [LoginStudio-class] object for the IQB Studio Lite
#'
#' @description
#' Provides a routine to login to an instance of the IQB Studio Lite.
#'
#' @param domain Character. Domain of the hosted instance of the IQB Studio Lite. Default is the IQB Studio Lite.
#' @param dialog Logical. Should the password be entered using RStudio dialogs (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param version Character. App version of the IQB Studio instance. Defaults to "5.2.1".
#'
#' @return An object of the [LoginStudio-class] class.
#' @export
#'
#' @examples
#' login <- createStudioLogin(domain = "https://iqb-testcenter.de/api")
#' workspace <- accessWorkspace(login = login, id = 125)
#'
#' @details
#' Calling the `createStudioLogin()` function generates the following curl request
#' on the `domain` (default is "https://www.iqb-studio.de/api/) with the `name`
#' and the `password` provided by the user:
#'
#' ```
#' curl --location --request POST '{domain}/api/login?username={name}&password={password}'
#' --header 'app-version: 5.2.1'
#' }'
#' ```
#' Note that the name and the password are only available to the function call
#' and cannot be accessed later as they are not part of the [Login-class] object generated.
#'
createStudioLogin <- function(domain = "https://www.iqb-studio.de/api",
                              dialog = TRUE,
                              version = "5.2.1", ...) {
  cli_setting()

  test_mode <- getOption("eatPrepTBA.test_mode")

  if (is.null(test_mode) || ! test_mode) {
    isRStudio <- Sys.getenv("RSTUDIO") == "1"

    if (isRStudio & dialog) {
      name <- rstudioapi::askForPassword("Enter your username: ")
      password <- rstudioapi::askForPassword("Enter your password: ")
    } else {
      name <- readline(prompt = "Enter your username: ")
      password <- readline(prompt = "Enter your password: ")
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

  headers = c(
    "app-version" = version
  )

  request <- httr::POST(url = glue::glue("{domain}/login?username={credentials$name}&password={credentials$password}"),
                  httr::add_headers(headers))

  if (request$status_code == 201) {
    response <- stringr::str_remove_all(httr::content(request, as = "text"), "\"")
    token <- glue::glue("Bearer {response}")

    headers <- c(headers,
                 "Authorization" = as.character(token))

    request_workspaces <- httr::GET(url = glue::glue("{domain}/auth-data"),
                              httr::add_headers(headers))

    ws_group_ids <-
      httr::content(request_workspaces)$workspaces %>%
      purrr::map_depth(.depth = 1, "id")

    workspaces <-
      httr::content(request_workspaces)$workspaces %>%
      purrr::map_depth(.depth = 1, "workspaces")

    ws_ids <- purrr::map_depth(.depth = 2, workspaces, "id") %>% unlist() %>% as.character()
    ws_labels <- purrr::map_depth(.depth = 2, workspaces, "name") %>% unlist()

    names(ws_ids) <- ws_labels

    Login <- new("LoginStudio",
                 domain = domain,
                 token = token,
                 version = version,
                 workspace = ws_ids
    )

    cli::cli_alert_success("Login was successful.")
    cli::cli_text("A token was generated to access the following workspaces ({.ws-id id}: {.ws label}):")
    cli::cli_li(items = glue::glue("{{.ws-id {Login@workspace}}}: {{.ws {names(Login@workspace)}}}"))
    cli::cli_alert_info("Please note that the login becomes invalid if you log in to the Testcenter manually.")


    return(invisible(Login))
  } else {
    cli::cli_alert_danger("Login was not successful. Please check if you have admin rights or are already logged in on a browser.")

    return(NULL)
  }
}
