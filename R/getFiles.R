setGeneric("getFiles", function(workspace, ...) {
  standardGeneric("getFiles")
})

setMethod("getFiles",
          signature = signature(workspace = "Workspace"),
          function(workspace, id = NULL, type, listFun, getFun) {
            cli_setting()

            ws_file_ids_all <- listFun(workspace)
            if (is.null(id)) {
              ws_file_ids <- ws_file_ids_all
            } else {
              ws_file_ids <- intersect(id, ws_file_ids_all)
              n_ws_file_ids <- length(ws_file_ids)

              if (n_ws_file_ids == 0) {
                msg <- glue::glue("No {{.{type} {{type}}}} information could be retrieved for: {{.{type} {{id}}}}")
                cli::cli_abort(msg)
              }

              ws_file_not <- setdiff(id, ws_file_ids_all)
              n_ws_file_not <- length(ws_file_not)
              if (length(ws_file_not) >= 1) {
                msg <- glue::glue("No {{.{type} {{type}}}} information could be retrieved for {{n_ws_file_not}} {{type}}{{?s}}: {{.{type} {{ws_file_not}}}}")
                cli::cli_alert_danger(msg)
              }
            }

            ws_files_tbl <-
              tibble::enframe(ws_file_ids, name = NULL, value = "id") %>%
              dplyr::mutate(
                resource = purrr::map(id,
                                      .f = function(x) getFun(workspace = workspace, id = x),
                                      .progress = list(name = glue::glue("Downloading {type} information")))
              ) %>%
              tidyr::unnest(resource)

            return(ws_files_tbl)
          })
