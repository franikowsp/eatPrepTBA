#' Get logs
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved  or all groups if not specified.
#'
#' @description
#' This function returns logs from the IQB Testcenter
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_logs,WorkspaceTestcenter-method
setGeneric("get_logs", function(workspace, groups = NULL) {
  standardGeneric("get_logs")
})

#' @describeIn get_logs Get responses of a given Testcenter workspace
setMethod("get_logs",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # if (is.null(groups)) {
            #   testtakers <- getTesttakers(workspace)
            #   groups <- unique(testtakers$groupname)
            # }
            #
            #
            # params <- list(
            #   dataIds = glue::glue_collapse(groups, sep = ",")
            # )

            resp <-
              base_req(method = "GET", endpoint = c("workspace", ws_id, "report", "log"),
                     query = c(dataIds = groups)) %>%
              httr2::req_perform()

            resp %>%
              httr2::resp_body_json() %>%
              # Rectangularize (zu tibble)
              tibble::enframe(name = NULL) %>%
              # Schleife zum Spreaden der Einträge (Auslesen in tibble)
              dplyr::mutate(
                # Ladebalken?
                value = purrr::map(value, \(x) tibble::as_tibble(x))
              ) %>%
              # Entpacken
              tidyr::unnest(value)

            ### TODO: tryCatch
            return(response_json)
          })
