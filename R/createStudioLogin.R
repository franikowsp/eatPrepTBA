#' Generate a [LoginStudio-class] object for the IQB Studio Lite
#'
#' @description
#' Provides a routine to login to an instance of the IQB Studio Lite.
#'
#' @param domain Character. Domain of the hosted instance of the IQB Studio Lite. Default is the IQB Studio Lite.
#' @param dialog Logical. Should the password be entered using RStudio dialogs (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param version Character. App version of the IQB Studio instance. Defaults to "6.1.0".
#' @param keyring Logical. Should the [keyring] package be used to save the passkey? This saves your credentials to your local machine.
#' @param keyring Logical. If your password on the domain has changed - should the [keyring] password be changed?
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
#' --header 'app-version: 6.1.0'
#' }'
#' ```
#' Note that the name and the password are only available to the function call
#' and cannot be accessed later as they are not part of the [Login-class] object generated.
#' %>%
createStudioLogin <- function(domain = "https://www.iqb-studio.de/api",
                              dialog = TRUE,
                              version = "7.3.1",
                              keyring = FALSE,
                              changeKey = FALSE,
                              verbose = TRUE,
                              ...) {
  cli_setting()

  credentials <- getCredentials(domain = domain,
                                dialog = dialog,
                                keyring = keyring,
                                changeKey = changeKey,
                                encode = TRUE)

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

    ws_groups <- httr::content(request_workspaces)$workspaces

    ws_group_ids <-
      ws_groups %>%
      purrr::map(function(x) {
        group_id <- x$id

        names <-
          x$workspaces %>%
          purrr::map("name") %>%
          unlist()

        ids <-
          x$workspaces %>%
          purrr::map("id") %>%
          unlist() %>%
          as.character()

        names(ids) <- names

        ws_groups <-
          list(
          id = group_id,
          workspaces = ids
        )
      })

    ws_group_labels <-
      ws_groups %>%
      purrr::map_depth(.depth = 1, "name")

    names(ws_group_ids) <- ws_group_labels

    ws_ids <-
      ws_group_ids %>%
      purrr::map("workspaces") %>%
      purrr::reduce(c)

    Login <- new("LoginStudio",
                 domain = domain,
                 token = token,
                 version = version,
                 workspace = ws_ids,
                 workspace_groups = ws_group_ids
    )

    cli::cli_alert_success("Login was successful.")

    if (verbose) {
      show(Login)
    }

    return(invisible(Login))
  } else {
    cli::cli_alert_danger("Login was not successful. Please check if you have admin rights or are already logged in on a browser.")

    return(NULL)
  }
}
