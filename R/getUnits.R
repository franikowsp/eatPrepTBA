setGeneric("getUnits", function(workspace, ...) {
  standardGeneric("getUnits")
})

setMethod("getUnits",
          signature = signature(workspace = "Workspace"),
          function(workspace, id = NULL) {
            ws_unit_ids_all <- listUnits(workspace)
            if (is.null(id)) {
              ws_unit_ids <- ws_unit_ids_all
            } else {
              ws_unit_ids <- intersect(id, ws_unit_ids_all)

              ws_units_not <- setdiff(id, ws_unit_ids_all)
              n_ws_units_not <- length(ws_units_not)
              if (length(ws_units_not) >= 1) {
                cli::cli_alert_danger("No unit information could be retrieved for {n_ws_units_not} unit{?s}: {ws_units_not}")
              }
            }

            ws_units_tbl <-
              tibble::enframe(ws_unit_ids, name = NULL, value = "id") %>%
              dplyr::mutate(
                resource = purrr::map(id,
                                      .f = function(x) getUnit(workspace, x),
                                      .progress = list(name = "Downloading unit information"))
              ) %>%
              tidyr::unnest(resource)

            return(ws_units_tbl)
          })
