setMethod(f = "show",
          signature = "Login",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the Testcenter and allows to access the following workspaces ({.ws-id id}: {.ws label}):")
            cli::cli_li(items = glue::glue("{{.ws-id {object@workspace}}}: {{.ws {names(object@workspace)}}}"))

            cli::cli_alert_info("Please note that the login becomes invalid if you log in to the Testcenter manually.")
          })

setMethod(f = "show",
          signature = "Workspace",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the workspace {.ws {object@label}} (id {.ws-id {object@id}}).")

          })
