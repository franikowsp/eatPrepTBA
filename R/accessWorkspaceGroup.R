#' Access a workspace group
#'
#' @param login Login.
#'
#' @return [WorkspaceStudio-class] object.
#' @export
#'
#' @examples
#' login <- createLogin(domain = "https://iqb-testcenter.de/api")
#' workspace <- accessWorkspaceGroup(login = login, id = 125)
#' @aliases
#' accessWorkspaceGroup,LoginTestcenter-method,LoginStudio-method
setGeneric("accessWorkspaceGroup", function(login, ...) {
  cli_setting()
  standardGeneric("accessWorkspaceGroup")
})

#' @describeIn accessWorkspaceGroup Provide access to a selected workspace after logging in
#'
#' @param id Numeric. Workspace group id (see URL).
#' @param label Character. Workspace group label.
setMethod("accessWorkspaceGroup",
          signature = signature(login = "LoginStudio"),
          function(login, id = NULL, label = NULL, verbose = TRUE) {
            cli_setting()

            wsg_ids <- purrr::map_int(login@workspace_groups, "id")
            wsg_labels <- names(login@workspace_groups)

            if (is.null(label) && is.null(id)) {
              cli::cli_abort("Either workspace group {.wsg label} or {.wsg-id id} must be defined.")
            }

            if (!is.null(label)) {
              if (!label %in% wsg_labels) {
                cli::cli_abort("Provided workspace provided {.wsg label} ({.wsg {label}}) does not match any workspace label on IQB Studio. Please check.")
              }

              wsg_label <- label
              wsg_id <- login@workspace_groups[[label]]$id

              if (!is.null(id) && ws_id != id) {
                cli::cli_abort("Provided workspace {.wsg-id id} ({.wsg-id {id}}) does not match workspace id on IQB Studio ({.wsg-id {ws_id}}). Please check.")
              }
            }

            if (!is.null(id)) {
              if (!id %in% wsg_ids) {
                cli::cli_abort("Provided workspace {.wsg-id id} ({.wsg-id {id}}) does not match any workspace group id on IQB Studio. Please check.")
              }

              wsg_id <- id
              wsg_label <- names(wsg_ids[wsg_ids == id])

              if (!is.null(label) && ws_label != label) {
                cli::cli_abort("provided workspace {.wsg label} ({.wsg {label}}) does not match workspace label on IQB Studio ({.wsg {ws_label}}). Please check.")
              }
            }

            workspaces <- login@workspace_groups[[wsg_label]]$workspaces

            workspace_group <-
              purrr::map(workspaces, function(ws_id) {
                accessWorkspace(login, id = ws_id, verbose = FALSE)
              })

            # wsg <-
            #   login@workspace_groups %>%
            #   purrr::keep(function(x) id %in% x$workspaces) %>%
            #   purrr::map("id") %>%
            #   purrr::list_simplify()
            #
            # workspace <- new("WorkspaceStudio",
            #                  login = login,
            #                  id = as.numeric(ws_id),
            #                  label = ws_label,
            #                  group_id = as.numeric(wsg),
            #                  group_label = names(wsg)
            # )

            cli::cli_alert_success("Workspace group can be accessed with the generated object.")

            if (verbose) {
              show(workspace_group)
            }

            return(workspace_group)
          })
# stu_wsg[[1]] %>% getUnits()
