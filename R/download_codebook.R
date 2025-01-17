#' Download codebook
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download codebook via the API.
#' @param path Character. Path for the codebook file to be downloaded.
#' @param unit_keys Character. Keys (short names) of the units in the workspace the codebook should be retrieved from. If set to `NULL` (default), the codebook will be generated for the all units.
#' @param format Character. Either `"docx"` (default) or `json`.
#' @param missings_profile Missings profile. (Currently without effect.)
#' @param only_coded Logical. Should only variables with codes be shown?
#' @param general_instructions Logical. Should the general coding instructions be printed? Defaults to `TRUE`.
#' @param hide_item_var_relation Logocal. Should item-variable relations be printed? Defaults to `FALSE`.
#' @param derived Logical. Should the derived variables be printed? Defaults to `TRUE`.
#' @param manual Logical. Should only items with manual coding be printed? Defaults to `TRUE`.
#' @param closed Logical. Should items that could be automatically coded be printed? Defaults to `TRUE`.
#' @param show_score Logical. Should the score be printed? Defaults to `FALSE`.
#' @param code_label_to_upper Logical. Should the code labels be printed in capital letters? Defaults to `TRUE`.
#'
#' @description
#' This function downloads codebooks from the IQB Studio.
#'
#' @return NULL
#' @export
#'
#' @aliases
#' download_codebook,WorkspaceStudio-method
setGeneric("download_codebook", function(workspace,
                                         path,
                                         unit_keys = NULL,
                                         # Exportformat
                                         format = "docx",
                                         # Missings-Profil
                                         missings_profile = NULL,
                                         # Nur Variablen mit Codes
                                         only_coded = FALSE,
                                         # Allgemeine Hinweise für jede Variable
                                         general_instructions = TRUE,
                                         # Item-Variable-Relation für jede Variable
                                         hide_item_var_relation = TRUE,
                                         # Abgeleitete Variablen
                                         derived = TRUE,
                                         # Manuell kodierte Variablen
                                         manual = TRUE,
                                         # Geschlossen kodierte Variablen
                                         closed = TRUE,
                                         # Bewertung anzeigen
                                         show_score = FALSE,
                                         # Code-Label in Großbuchstaben
                                         code_label_to_upper = TRUE) {
  cli_setting()

  standardGeneric("download_codebook")
})

#' @describeIn download_codebook Upload a file in a defined workspace
setMethod("download_codebook",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   path,
                   unit_keys = NULL,
                   format = "docx",
                   missings_profile = NULL,
                   only_coded = FALSE,
                   general_instructions = TRUE,
                   hide_item_var_relation = TRUE,
                   derived = TRUE,
                   manual = TRUE,
                   closed = TRUE,
                   show_score = FALSE,
                   code_label_to_upper = TRUE) {
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

            unit_ids <-
              units %>%
              purrr::map("unit_id") %>%
              purrr::list_simplify()

            # Normalize query params
            query_params <-
              list(
                format = format,
                hasOnlyVarsWithCodes = only_coded,
                generalInstructions = general_instructions,
                hideItemVarRelation = hide_item_var_relation,
                derived = derived,
                onlyManual = manual,
                closed = closed,
                showScore = show_score,
                codeLabelToUpper = code_label_to_upper
              ) %>%
              purrr::map(stringr::str_to_lower)

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c(
                         "download",
                         "docx",
                         "workspaces",
                         ws_id,
                         "coding-book",
                         stringr::str_c(unit_ids, collapse = ",")
                       ),
                       query = query_params) %>%
                httr2::req_perform(path = path)

              cli::cli_alert_success("Codebook of
              workspace {.ws-id {ws_id}}: {.ws-label {ws_label}}
              was successfully downloaded to {.file {path}}", wrap = TRUE)
            }

            run_safe(run_req,
                     error_message = "Codebook could not be generated. Please check if you have already,
                     opened {.file {path}} (that migh cause the error).")
          })
