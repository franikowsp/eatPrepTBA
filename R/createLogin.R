# Create a function to initialize the object with user input
createLogin <- function(domain = "https://iqb-testcenter.de/api") {
  isRStudio <- Sys.getenv("RSTUDIO") == "1"

  if (isRStudio) {
    name <- rstudioapi::askForPassword("Enter your user name: ")
    password <- rstudioapi::askForPassword("Enter your user password: ")
  } else {
    name <- readline(prompt = "Enter your user name: ")
    password <- readline(prompt = "Enter your user password: ")
  }

  credentials <- list(
    name = name,
    password = password
  )

  # Generates a request like that: ------------------------------------------
  # curl --location --request PUT '{domain}/session/admin' \
  # --header 'Content-Type: application/json' \
  # --data '{
  #     "name": "{name}",
  #     "password": "{password}"
  # }'

  request <- httr::PUT(glue::glue("{domain}/session/admin"),
                       config = httr::content_type_json(),
                       body = jsonlite::toJSON(credentials, auto_unbox = TRUE))

  if (request$status_code == 200) {
    request_content <- httr::content(request)
    claims_workspace_admin <- request_content$claims$workspaceAdmin

    ws_labels <- purrr::map_chr(claims_workspace_admin, "label")
    ws_ids <- purrr::map_chr(claims_workspace_admin, "id")

    names(ws_ids) <- ws_labels

    Login <- new("Login",
                 domain = domain,
                 token = request_content$token,
                 access = ws_ids
    )

    cli::cli_alert_success("Login was successful.")
    cli::cli_text("A token was generated to access the following workspaces:")
    cli::cli_li(items = names(Login@access))

    return(Login)
  } else {
    cli::cli_alert_danger("Login was not successful. Please check if you have admin rights.")

    return(NULL)
  }
}
