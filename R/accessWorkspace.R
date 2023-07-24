setGeneric("accessWorkspace", function(login, ...) {
  standardGeneric("accessWorkspace")
})

setMethod("accessWorkspace",
          signature = signature(login = "Login"),
          function(login, id = NULL, label = NULL) {

            if (is.null(label) && is.null(id)) {
              cli::cli_abort("Either workspace {.arg label} or {.arg id} must be defined.")
            }

            if (!is.null(label)) {
              ws_label <- label
              ws_id <- login@access[[label]]

              if (!is.null(id) && ws_id != id) {
                cli::cli_abort("Workspace {.arg id} provided ({id}) does not match workspace id on test center ({ws_id}). Please check.")
              }
            }

            if (!is.null(id)) {
              ws_id <- id
              ws_label <- names(login@access[login@access == id])

              if (!is.null(label) && ws_label != label) {
                cli::cli_abort("Workspace {.arg label} provided ({label}) does not match workspace label on test center ({ws_label}). Please check.")
              }
            }

            Workspace <- new("Workspace",
                             login = login,
                             id = ws_id,
                             label = ws_label)

            return(Workspace)
          })
