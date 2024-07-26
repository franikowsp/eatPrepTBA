#' Add a comment for a unit
#'
#' @description
#' This function adds an entry in the comment tab of the Studio of a single unit.
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_id Unit ID to be changed.
#' @param comment Comment to be added to the unit. Should contain html markup.
#'
#' @return NULL
#'
#' @aliases
#' add_comment,WorkspaceStudio-method
#'
#' @keywords internal
setGeneric("add_comment", function(workspace,
                                   unit_id,
                                   comment = NULL) {
  cli_setting()

  standardGeneric("add_comment")
})


#' @describeIn add_comment Add a comment in a defined workspace
setMethod("add_comment",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_id,
                   comment = NULL) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            user_id <- workspace@login@user_id
            user_label <- workspace@login@user_label

            body <-
              list(
                body = comment,
                userName = user_label,
                userId = user_id,
                parentId = NULL,
                unitId = unit_id
              )

            run_req <- function() {
              base_req(method = "POST",
                       endpoint = c("workspace", ws_id, unit_id, "comments")) %>%
                httr2::req_body_json(data = body, auto_unbox = TRUE) %>%
                httr2::req_perform()
            }

            # For the error message
            url <- glue::glue("https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/properties")

            run_safe(run_req,
                     error_message = "Comment could not be added for
                       unit with id {unit_id} {.url {url}}.")

          })
