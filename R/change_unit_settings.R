#' Change unit settings
#'
#' @description
#' This function updates the Studio metadata, i.e., versions for player, editor, or schemer, and the groups and the states of a single unit. To change multiple units, use [change_units_settings()].
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_id Unit ID to be changed.
#' @param player New player version.
#' @param editor New editor version.
#' @param schemer New schemer version.
#' @param group_name New group name.
#' @param state New state.
#'
#' @return NULL
#'
#' @aliases
#' change_unit_settings,WorkspaceStudio-method
#'
#' @keywords internal
setGeneric("change_unit_settings", function(workspace,
                                            unit_id,
                                            player = NULL,
                                            editor = NULL,
                                            schemer = NULL,
                                            group_name = NULL,
                                            state = NULL) {
  cli_setting()

  standardGeneric("change_unit_settings")
})


#' @describeIn change_unit_settings Get unit information and coding scheme in a defined workspace
setMethod("change_unit_settings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_id,
                   player = NULL,
                   editor = NULL,
                   schemer = NULL,
                   group_name = NULL,
                   state = NULL) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            body <-
              settings(
                workspace = workspace,
                unit_id = unit_id,
                player = player,
                editor = editor,
                schemer = schemer,
                group_name = group_name,
                state = state
              )

            run_req <- function() {
              base_req(method = "PATCH",
                       endpoint = c("workspaces", ws_id, "units", unit_id, "metadata")) %>%
                httr2::req_body_json(data = body, auto_unbox = TRUE) %>%
                httr2::req_perform()
            }

            # For the error message
            url <- glue::glue("https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/properties")

            message <- glue::glue("Settings could not be changed for unit with id {unit_id} {.url {url}}.")

            run_safe(run_req,
                     error_message = message)

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
