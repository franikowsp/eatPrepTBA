#' Get multiple booklets
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve booklets from the API.
#' @param id Character vector (optional). Names of the booklets to be retrieved. If no id is set, all booklets in the workspace are retrieved.
#'
#' @description
#' This function returns the booklet information for multiple booklets by repeatedly calling [getBooklet()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getBooklets,Workspace-method
setGeneric("getBooklets", function(workspace, ...) {
  standardGeneric("getBooklets")
})

#' @describeIn getBooklets Get multiple booklets in a defined workspace
setMethod("getBooklets",
          signature = signature(workspace = "Workspace"),
          function(workspace, id = NULL) {
            getFiles(
              workspace = workspace,
              id = id,
              type = "booklet",
              listFun = listBooklets,
              getFun = getBooklet
            )

            # TODO: Further preparation, e.g.,
            # units$xml[[10]]$Metadata[[1]] %>%
            # purrr::list_simplify() %>%
            #   tibble::enframe() %>%
            #   tidyr::pivot_wider(values_fn = as.character)

          })
