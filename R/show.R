setMethod(f = "show",
          signature = "Login",
          definition = function(object) {
            cli::cli_text("This object provides a valid login for data retrieval from the IQB-Testcenter.")
            cli::cli_text("Please not that the login becomes invalid if you log-in to the test center manually.")
          })

setMethod(f = "show",
          signature = "Workspace",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the workspace {.ws {object@label}} (id {.ws-id {object@id}}) of the IQB-Testcenter.")
          })
