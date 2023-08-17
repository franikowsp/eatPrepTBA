#' List files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve file list from the API.
#'
#' @description
#' Please note the wrapper functions [listUnits()], [listBooklets()], and [listTesttakers()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' listFiles,Workspace-method
setGeneric("listFiles", function(workspace, ...) {
  standardGeneric("listFiles")
})

#' @describeIn listFiles List all files in a defined workspace
#'
#' @param type Character (optional). Type of the files to retrieve from the API. If no type is specified, all files are listed.
setMethod("listFiles",
          signature = signature(workspace = "Workspace"),
          function(workspace, type = NULL) {
            headers = c(
              AuthToken = workspace@login@token
            )

            domain <- workspace@login@domain
            ws_id <- workspace@id

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
