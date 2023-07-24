setGeneric("listResources", function(workspace) {
  standardGeneric("listResources")
})

setMethod("listResources",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_resources <- listFiles(workspace, type = "Resource")

            ws_resources %>%
              dplyr::filter(stringr::str_detect(name, "\\.voud") |
                              stringr::str_detect(name, "\\.vocs")) %>%
              dplyr::pull("name")
          })


