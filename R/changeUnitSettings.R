#' Change unit settings
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param id Character. Name of the unit to be changed
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [changeUnitsSettings()].
#'
#' @return A tibble.
#'
#' @export
#'
#' @examples
#' @aliases
#' getUnit,WorkspaceStudio-method
setGeneric("changeUnitSettings", function(workspace, id, ...) {
  standardGeneric("changeUnitSettings")
})


#' @describeIn changeUnitSettings Get unit information and coding scheme in a defined workspace
setMethod("changeUnitSettings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, id, ...) {
            headers = c(
              Authorization = workspace@login@token,
              "app-version" = workspace@login@version,
              "Content-Type" = "application/json"
            )

            domain <- workspace@login@domain
            ws_id <- workspace@id
            ws_label <- workspace@label

            body <- settings(
              workspace,
              id,
              ...
            )

            # Request
            response <-
              httr::PATCH(url = glue::glue(
                "{domain}/workspace/{ws_id}/{id}/metadata"
              ),
              config = httr::add_headers(.headers = headers),
              body = jsonlite::toJSON(body, auto_unbox = TRUE)
              )

            if (response$status_code != 200) {
              url <- glue::glue("https://www.iqb-studio.de/#/a/{ws_id}/{id}/properties")
              cli::cli_alert_danger("Settings could not be changed for unit with id {id} {.url {url}}.")
            }

          })

settings <- function(workspace, id, ...) {
  set <-
    list(
      id = id,
      ...
    )

  # This approach ensures that no other arguments become valid
  set_protect <-
    list(
      id = id
    )

  if (!is.null(set$player) & is.numeric(set$player)) {
    set_protect$player <- glue::glue("iqb-player-aspect@{set$player}")
  }

  if (!is.null(set$editor) & is.numeric(set$editor)) {
    set_protect$editor <- glue::glue("iqb-editor-aspect@{set$editor}")
  }

  if (!is.null(set$schemer) & is.numeric(set$schemer)) {
    set_protect$schemer <- glue::glue("iqb-schemer@{set$schemer}")
  }

  if (!is.null(set$groupName)) {
    group_names <- c(listGroups(workspace), "")

    if (!set$groupName %in% group_names) {
      set_protect$groupName <- NULL
      cli::cli_alert_danger("groupName = {set$groupName} is not a valid group name for this workspace")
    } else {
      set_protect$groupName <- set$groupName
    }
  }

  if (!is.null(set$state)) {
    states <- getStates(workspace)

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
