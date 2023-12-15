#' Generate a [Login-class] object for the IQB Testcenter
#'
#' @description
#' Provides a routine to login to an instance of the IQB Testcenter.
#'
#' @param domain Character. Domain of the hosted instance of the IQB Testcenter. Default is the IQB Testcenter.
#' @param dialog Logical. Should the password be entered using RStudio dialogs (`TRUE`) or using the console (`FALSE`). Defaults to `TRUE`.
#'
#' @return An object of the [LoginTestcenter-class] class.
#' @export
#'
#' @examples
#' login <- createTestcenterLogin(domain = "https://iqb-testcenter.de/api")
#' workspace <- accessWorkspace(login = login, id = 125)
#'
#' @details
#' Calling the `createTestcenterLogin()` function generates the following curl request
#' on the `domain` (default is https://iqb-testcenter.de/api) with the `name` and
#' the `password` provided by the user:
#'
#' ```
#' curl --location --request PUT '{domain}/session/admin'
#' --header 'Content-Type: application/json'
#' --data '{
#'     "name": "{name}",
#'     "password": "{password}"
#' }'
#' ```
#' Note that the name and the password are only available to the function call
#' and cannot be accessed later as they are not part of the [Login-class] object generated.
#'
createTestcenterLogin <- function(domain = "https://iqb-testcenter.de/api",
                                  dialog = TRUE,
                                  keyring = FALSE,
                                  changeKey = FALSE,
                                  ...) {
  cli_setting()

  credentials <- getCredentials(domain = domain,
                                dialog = dialog,
                                keyring = keyring,
                                changeKey = changeKey)

  request <- httr::PUT(glue::glue("{domain}/session/admin"),
                       config = httr::content_type_json(),
                       body = jsonlite::toJSON(credentials, auto_unbox = TRUE))

  if (request$status_code == 200) {
    request_content <- httr::content(request)
    claims_workspace_admin <- request_content$claims$workspaceAdmin

    ws_labels <- purrr::map_chr(claims_workspace_admin, "label")
    ws_ids <- purrr::map_chr(claims_workspace_admin, "id")

    names(ws_ids) <- ws_labels

    Login <- new("LoginTestcenter",
                 domain = domain,
                 token = request_content$token,
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
