setMethod(f = "show",
          signature = "Login",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the Testcenter and allows to access the following workspaces ({.ws-id id}: {.ws label}):")
            cli::cli_li(items = glue::glue("{{.ws-id {object@workspace}}}: {{.ws {names(object@workspace)}}}"))

            cli::cli_alert_info("Please note that the login becomes invalid if you log in to the Testcenter manually.")
          })

setMethod(f = "show",
          signature = "LoginStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the IQB Studio and allows to access the following workspace groups ({.wsg-id id}: {.wsg label}) with their respective workspaces ({.ws-id id}: {.ws label}):")
            ws_groups <-
              object@workspace_groups %>%
              purrr::map("id") %>%
              purrr::list_simplify()

            ul <- cli::cli_ul()
            object@workspace_groups %>%
              purrr::imap(function(x, i) {
                cli::cli_li(glue::glue("{{.wsg-id {x$id}}}: {{.wsg {i}}}"))

                ul_nest <- cli::cli_ul()
                cli::cli_li(items = glue::glue("{{.ws-id {x$workspaces}}}: {{.ws {names(x$workspaces)}}}"))
                cli::cli_end(ul_nest)
              })
            cli::cli_end(ul)
          })

# setMethod(f = "show",
#           signature = "Workspace",
#           definition = function(object) {
#             cli_setting()
#
#             cli::cli_text("This object provides a valid login for data retrieval from the workspace {.ws {object@label}} (id {.ws-id {object@id}}).")
#
#           })

setMethod(f = "show",
          signature = "WorkspaceStudio",
          definition = function(object) {
            cli_setting()

            cli::cli_text("This object provides a valid login for data retrieval from the workspace {.ws {object@label}} (id {.ws-id {object@id}}) that is part of the workspace group {.wsg {object@group_label}} (id {.wsg-id {object@group_id}}).")

          })
