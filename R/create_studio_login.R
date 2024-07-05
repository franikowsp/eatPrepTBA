#' Generate a [LoginStudio-class] object for the IQB Studio Lite
#'
#' @description
#' Provides a routine to login to an instance of the IQB Studio Lite.
#'
#' @param base_url Character. Base URL of the hosted instance of the IQB Studio Lite. Default is the IQB Studio Lite.
#' @param version Character. App version of the IQB Studio instance. Defaults to "7.6.0".
#' @param keyring Logical. Should the [keyring] package be used to save the passkey? This saves your credentials to your local machine. Defaults to `FALSE`.
#' @param keyring Logical. If your password on the domain has changed - should the [keyring] password be changed? Defaults to `FALSE`.
#' @param dialog Logical. Should the password be entered using the RStudio dialog (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#' @param verbose Logical. If `TRUE`, additional information is printed. Defaults to `FALSE`.
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
create_studio_login <- function(base_url = "https://www.iqb-studio.de/",
                                version = "7.6.0",
                                keyring = FALSE,
                                change_key = FALSE,
                                dialog = TRUE,
                                verbose = FALSE) {
  cli_setting()

  credentials <- get_credentials(base_url = base_url,
                                 keyring = keyring,
                                 change_key = change_key,
                                 dialog = dialog)

  token <-
    httr2::request(base_url = base_url) %>%
    httr2::req_url_path_append("api", "login") %>%
    httr2::req_headers("app-version" = version) %>%
    httr2::req_method("POST") %>%
    httr2::req_url_query(username = credentials$name, password = credentials$password) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    stringr::str_remove_all("\"")

  auth_token <- glue::glue("Bearer {token}")

  base_req <- generate_base_req(base_url = base_url,
                                auth_token = auth_token,
                                version = version)
  # Perform request
  resp <-
    tryCatch(
      error = function(cnd) {
        cli::cli_alert_danger("Login was not successful.
                              Please check if you have admin rights or
                              are already logged in on a browser.",
                              wrap = TRUE)

        cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")


      },
      base_req(method = "GET",
               endpoint = "auth-data",
               query = list()) %>%
        # httr2::req_error() %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()
    )

  # Process HTTP response to ws group list and ws list
  wsg_list <-
    resp %>%
    purrr::pluck("workspaces") %>%
    purrr::map(function(wsg) {
      # Streamline workspace groups
      list(
        id = wsg$id,
        label = wsg$name,
        ws_list = wsg$workspaces
      )
    }) %>%
    purrr::map(function(wsg) {
      # Streamline workspaces (overwrite list entries)
      wsg$ws_list <-
        wsg$ws_list %>%
        purrr::map(function(ws) {
          list(
            id = ws$id,
            label = ws$name
          )})

      wsg
    })

  ws_list <-
    wsg_list %>%
    purrr::map("workspaces") %>%
    purrr::list_flatten()

  # Initialize Login object
  Login <- new("LoginStudio",
               base_url = base_url,
               auth_token = auth_token,
               base_req = base_req,
               ws_list = ws_list,
               wsg_list = wsg_list,
               version = version
  )

  cli::cli_alert_success("Login was successful.")

  if (verbose) {
    cli::cli_text("You are logged in to the IQB Studio Lite at {.url {base_url}} as {.user-label {resp$userName}}.")

    show(Login)
  }

  return(invisible(Login))
}
