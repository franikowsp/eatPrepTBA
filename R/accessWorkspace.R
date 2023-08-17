#' Access a workspace
#'
#' @param login Login.
#'
#' @return [Workspace-class] object.
#' @export
#'
#' @examples
#' login <- createLogin(domain = "https://iqb-testcenter.de/api")
#' workspace <- accessWorkspace(login = login, id = 125)
#' @aliases
#' accessWorkspace,Login-method
setGeneric("accessWorkspace", function(login, ...) {
  cli_setting()
  standardGeneric("accessWorkspace")
})

#' @describeIn accessWorkspace Provide access to a selected workspace after loggin in
#'
#' @param id Numeric. Workspace id (see URL).
#' @param label Character. Workspace label.
# TODO: Refactor cli messages. Should return more informative message when label and id don't match.
setMethod("accessWorkspace",
          signature = signature(login = "Login"),
          function(login, id = NULL, label = NULL) {

            if (is.null(label) && is.null(id)) {
              cli::cli_abort("Either workspace {.ws label} or {.ws-id id} must be defined.")
            }

            if (!is.null(label)) {
              if (!label %in% names(login@workspace)) {
                cli::cli_abort("Provided workspace provided {.ws label} ({.ws {label}}) does not match any workspace label on test center. Please check.")
              }

              ws_label <- label
              ws_id <- login@workspace[[label]]

              if (!is.null(id) && ws_id != id) {
                cli::cli_abort("Provided workspace {.ws-id id} ({.ws-id {id}}) does not match workspace id on test center ({.ws-id {ws_id}}). Please check.")
              }
            }

            if (!is.null(id)) {
              if (!id %in% login@workspace) {
                cli::cli_abort("Provided workspace {.ws-id id} ({.ws-id {id}}) does not match any workspace id on test center. Please check.")
              }

              ws_id <- id
              ws_label <- names(login@workspace[login@workspace == id])

              if (!is.null(label) && ws_label != label) {
                cli::cli_abort("provided workspace {.ws label} ({.ws {label}}) does not match workspace label on test center ({.ws {ws_label}}). Please check.")
              }
            }

            Workspace <- new("Workspace",
                             login = login,
                             id = as.numeric(ws_id),
                             label = ws_label)

            cli::cli_alert_success("Workspace {.ws-id {ws_id}}: {.ws {ws_label}} can be accessed with the generated object.")

            return(Workspace)
          })
