#' Download units
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download files via the API.
#' @param path Character. Path for the zip file to be downloaded.
#' @param unit_keys Character. Keys (short names) of the units in the workspace that should be downloaded. If set to `NULL` (default), all units in the workspace will be downloaded.
#' @param add_players Logical. Should the resepective Aspect Player(s) of the selected units be added? Defaults to `TRUE`.
#' @param add_testtakers_review Numeric. Number of Testcenter review logins (`run-review`). Defaults to `0`.
#' @param add_testtakers_monitor Numeric. Number of Testcenter monitor logins (`monitor-group`). Defaults to `0`.
#' @param add_testtakers_hot Numeric. Number of Testcenter testtaker logins (`run-review`). Defaults to `0`.
#' @param password_less Logical. Should passwords be added to the logins? Defaults to `FALSE`.
#' @param booklet_settings List. Settings for booklet parameters. Please refer to: https://pages.cms.hu-berlin.de/iqb/testcenter/pages/booklet-config.html
#'
#' @description
#' This function downloads units from the IQB Studio Lite.
#'
#' @return NULL
#' @export
#'
#' @aliases
#' download_units,WorkspaceStudio-method
setGeneric("download_units", function(workspace,
                                      path,
                                      unit_keys = NULL,
                                      add_players = TRUE,
                                      add_testtakers_review = 0,
                                      add_testtakers_monitor = 0,
                                      add_testtakers_hot = 0,
                                      password_less = FALSE,
                                      booklet_settings = list()) {
  cli_setting()
  standardGeneric("download_units")
})

setMethod("download_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   path,
                   unit_keys = NULL,
                   add_players = TRUE,
                   add_testtakers_review = 0,
                   add_testtakers_monitor = 0,
                   add_testtakers_hot = 0,
                   password_less = FALSE,
                   booklet_settings = list()) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            ws_label <- workspace@ws_label

            # Prepare units (with or without filter)
            units <- list_units(workspace)

            # TODO: Add mechanism to detect units that are not in the workspace
            if (!is.null(unit_keys)) {
              units <-
                units %>%
                purrr::keep(function(x) x$unit_key %in% unit_keys)
            }

            # TODO: Keep in mind as this might become a body string in the future!
            settings <-
              list(
                unitIdList = units %>% purrr::map("unit_id"),
                addPlayers = add_players,
                addTestTakersReview = add_testtakers_review,
                addTestTakersMonitor = add_testtakers_monitor,
                addTestTakersHot = add_testtakers_hot,
                passwordLess = password_less,
                bookletSettings = booklet_settings) %>%
              jsonlite::toJSON(auto_unbox = TRUE)

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c(
                         "workspaces",
                         ws_id),
                       query = list(download = TRUE, settings = body)) %>%
                httr2::req_perform(path = path)

              cli::cli_alert_success("Units of
              workspace {.ws-id {ws_id}}: {.ws-label {ws_label}}
              were successfully downloaded to {.file {path}}", wrap = TRUE)
            }

            run_safe(run_req,
                     error_message = "Units could not be downloaded.")
          })
