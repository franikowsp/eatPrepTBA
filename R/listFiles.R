setGeneric("listFiles", function(workspace, ...) {
  standardGeneric("listFiles")
})

setMethod("listFiles",
          signature = signature(workspace = "Workspace"),
          function(workspace, type = NULL) {
            headers = c(
              AuthToken = workspace@login@token
            )

            request <- httr::GET(url = glue::glue(
              "{login@domain}/workspace/{ws_id}/files"
            ),
            httr::add_headers(.headers=headers)
            )

            response <-
              httr::content(request) %>%
              tibble::enframe(name = "type_filter")

            if (!is.null(type)) {
              response <-
                response %>%
                dplyr::filter(type_filter %in% type)
            }

            response %>%
              dplyr::select(-type_filter) %>%
              dplyr::mutate(
                value = purrr::map(value,
                                   \(x) purrr::list_transpose(x) %>%
                                     tibble::as_tibble())
              )  %>%
              tidyr::unnest(value)
          })
