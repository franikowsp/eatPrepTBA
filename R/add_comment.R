#' Add a comment for a unit
#'
#' @description
#' This function adds an entry in the comment tab of the Studio of a single unit.
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_id Unit ID to be changed.
#' @param comment Comment to be added to the unit.
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
                       unit with id {id} {.url {url}}.")

          })

settings <- function(workspace,
                     unit_id,
                     player,
                     editor,
                     schemer,
                     group_name,
                     state) {
  set <-
    list(
      player = player,
      editor = editor,
      schemer = schemer,
      group_name = group_name,
      state = state
    )

  # This approach ensures that no other arguments become valid
  set_protect <-
    list(
      id = unit_id
    )

  if (!is.null(set$player) & is.numeric(set$player)) {
    if (set$player == round(set$player)) {
      set$player <- stringr::str_glue("{set$player}.0")
    }

    set_protect$player <- glue::glue("iqb-player-aspect@{set$player}")
  }

  if (!is.null(set$editor) & is.numeric(set$editor)) {
    if (set$editor == round(set$editor)) {
      set$editor <- stringr::str_glue("{set$editor}.0")
    }

    set_protect$editor <- glue::glue("iqb-editor-aspect@{set$editor}")
  }

  if (!is.null(set$schemer) & is.numeric(set$schemer)) {
    if (set$schemer == round(set$schemer)) {
      set$schemer <- stringr::str_glue("{set$schemer}.0")
    }

    set_protect$schemer <- glue::glue("iqb-schemer@{set$schemer}")
  }

  if (!is.null(set$group_name)) {
    group_names <- c(list_groups(workspace), "")

    if (!set$group_name %in% group_names) {
      set_protect$groupName <- NULL
      cli::cli_alert_danger("group_name = {set$group_name} is not a valid group name for this workspace")
    } else {
      set_protect$groupName <- set$group_name
    }
  }

  if (!is.null(set$state)) {
    states <- get_states(workspace)

    if (is.null(states)) {
      cli::cli_alert_danger("There are no states defined for this for this workspace")
    } else {
      if (is.numeric(set$state)) {
        if (!set$state %in% states$id) {
          set_protect$state <- NULL
          cli::cli_alert_danger("state = {set$state} is not a valid state for this workspace")
        } else {
          set_protect$state <- set$state
        }
      } else {
        if (!set$state %in% states$label) {
          set_protect$state <- NULL
          cli::cli_alert_danger("state = {set$state} is not a valid state for this workspace")
        } else {
          set_protect$state <- states %>% dplyr::filter(label == set$state) %>% dplyr::pull(id)
        }
      }
    }
  }

  return(set_protect)
}

# settings(workspace, id = 1, state = 2)
