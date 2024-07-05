# setMethod(f = "show",
#           signature = "Login",
#           definition = function(object) {
#             cli_setting()
#
#             cli::cli_text("This object provides a valid login for data retrieval from the Testcenter and allows to access the following workspaces ({.ws-id id}: {.ws label}):")
#             cli::cli_li(items = glue::glue("{{.ws-id {object@workspace}}}: {{.ws {names(object@workspace)}}}"))
#
#             cli::cli_alert_info("Please note that the login becomes invalid if you log in to the Testcenter manually.")
#           })

setMethod(f = "show",
          signature = "LoginStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("You have access to the following workspace groups ({.wsg-id ws_id}: {.wsg ws_label}) with their respective workspaces ({.ws-id ws_id}: {.ws ws_label}):")

            ul <- cli::cli_ul()
            object@wsg_list %>%
              purrr::map(function(wsg) {
                cli::cli_li(glue::glue("{{.wsg-id {wsg$id}}}: {{.wsg {wsg$label}}}"))

                ul_nest <- cli::cli_ul()
                wsg$ws_list %>%
                  purrr::map(function(ws) {
                    cli::cli_li(items = glue::glue("{{.ws-id {ws$id}}}: {{.ws {ws$label}}}"))
                  })
                cli::cli_end(ul_nest)
              })
            cli::cli_end(ul)
          })

setMethod(f = "show",
          signature = "WorkspaceStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the workspace {.ws {object@label}} (id {.ws-id {object@id}}) that is part of the workspace group {.wsg {object@group_label}} (id {.wsg-id {object@group_id}}).")

          })
