setMethod(f = "show",
          signature = "Login",
          definition = function(object) {
            cli::cli_text("This object provides a valid login for data retrieval from the IQB-Testcenter.")
            cli::cli_text("Please not that the login becomes invalid if you log-in to the test center manually.")
          })

setMethod(f = "show",
          signature = "Workspace",
          definition = function(object) {
            cli::cli_text("This object provides a valid login for data retrieval from the workspace {object@label} (id {object@id}) the IQB-Testcenter.")
          })
