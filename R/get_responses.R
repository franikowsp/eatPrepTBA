#' Get responses directly from Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_responses,WorkspaceTestcenter-method
setGeneric("get_responses", function(workspace,
                                     groups = NULL) {
  cli_setting()

  standardGeneric("get_responses")
})

#' @describeIn get_responses Get responses of a given Testcenter workspace
setMethod("get_responses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "report", "response"),
                       query = list(dataIds = group)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            # resp <-
            #   run_safe(run_req,
            #            error_message = "Responses could not be retrieved.")

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading responses")

            if (!is.null(resp)) {
              responses_raw <-
                resp %>%
                purrr::flatten() %>%
                # Rectangularize (zu tibble)
                tibble::enframe(name = NULL) %>%
                # Schleife zum Spreaden der Einträge (Auslesen in tibble)
                dplyr::mutate(
                  # Ladebalken?
                  # TODO: Was genau fliegt hier raus?
                  value = purrr::map(value, function(x) {
                    x %>%
                      purrr::discard(is.null) %>%
                      tibble::as_tibble()
                  })
                ) %>%
                # Entpacken
                tidyr::unnest(value)

              # For legacy reasons, this has to be added
              # TODO: Can this be removed at a later point in time?
              if (tibble::has_name(responses_raw, "originalUnitId")) {
                unit_cols <- c(
                  unit_key = "originalUnitId",
                  unit_alias = "unitname"
                )

                responses_raw <-
                  responses_raw %>%
                  dplyr::mutate(
                    originalUnitId = ifelse(is.na(originalUnitId) | originalUnitId == "", unitname, originalUnitId)
                  )
              } else {
                unit_cols <- c(
                  unit_key = "unitname"
                )
              }

              responses_raw %>%
                dplyr::select(
                  dplyr::any_of(c(
                    group_id = "groupname",
                    login_name = "loginname",
                    login_code = "code",
                    booklet_id = "bookletname",
                    unit_cols,
                    responses_nest = "responses",
                    laststate_nest = "laststate"
                  ))
                ) %>%
                dplyr::group_by(
                  dplyr::across(dplyr::any_of(c("group_id", "login_name",
                                                "login_code", "booklet_id",
                                                "unit_key", "unit_alias")))
                ) %>%
                # TODO: Check for robustness of this fix!
                dplyr::filter(
                  !is.na(laststate_nest)
                ) %>%
                dplyr::summarise(
                  laststate_nest = unique(laststate_nest),
                  responses_nest = list(purrr::reduce(list(responses_nest), c))
                ) %>%
                dplyr::mutate(
                  responses_nest = purrr::map(responses_nest, unnest_responses_list,
                                              .progress = "Prepare responses"),
                  laststate_nest = purrr::map(laststate_nest, unnest_laststate,
                                              .progress = "Prepare state information")
                ) %>%
                tidyr::unnest(c("responses_nest", "laststate_nest"), keep_empty = TRUE) %>%
                dplyr::group_by(
                  dplyr::across(dplyr::any_of(c("file", "group_id", "login_name",
                                                "login_code", "booklet_id", "unit_key")))
                ) %>%
                tidyr::pivot_wider(
                  names_from = c("id"),
                  values_from = dplyr::any_of(c("content", "ts")),
                  names_glue = "{id}_{.value}"
                ) %>%
                dplyr::ungroup() %>%
                dplyr::rename(
                  dplyr::any_of(c(
                    responses = "elementCodes_content",
                    state_variables = "stateVariableCodes_content",
                    responses_ts = "elementCodes_ts",
                    state_variables_ts = "stateVariableCodes_ts",
                    player = "PLAYER",
                    presentation_progress = "PRESENTATION_PROGRESS",
                    response_progress = "RESPONSE_PROGRESS",
                    page_no = "CURRENT_PAGE_NR",
                    page_id = "CURRENT_PAGE_ID",
                    page_count = "PAGE_COUNT"
                  ))
                )
            } else {
              tibble::tibble()
            }
          })

# Function
# TODO: Hier noch die Funktionen vereinheitlichen aus der read_responses()-Funktion
unnest_responses_list <- function(json_parsed) {
  if (length(json_parsed) == 0) {
    return(tibble::tibble(
      id = "elementCodes",
      content = NA_character_
    ))
  }

  if ("lastSeenPageIndex" %in% purrr::map_chr(json_parsed, "id")) {
    return(tibble::tibble(
      id = "elementCodes",
      content = as.character(jsonlite::toJSON(json_parsed))
    ))
  } else {
    json_parsed %>%
      purrr::list_transpose() %>%
      tibble::as_tibble() %>%
      dplyr::select(
        dplyr::any_of(c(
          "id",
          "content",
          "ts"
        ))
      )
  }
}
