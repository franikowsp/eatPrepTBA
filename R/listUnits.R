setGeneric("listUnits", function(workspace) {
  standardGeneric("listUnits")
})

setMethod("listUnits",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_units <- listFiles(workspace, type = "Unit")

            ws_units %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
