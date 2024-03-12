#' Get reviews
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve reviews from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getReviews,Workspace-method
setGeneric("getReviews", function(workspace, groups = NULL) {
  standardGeneric("getReviews")
})

#' @describeIn getReviews Get responses of a defined workspace
setMethod("getReviews",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            if (is.null(groups)) {
              testtakers <- getTesttakers(workspace)
              groups <- unique(testtakers$groupname)
            }

            params = list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            # Read response JSONs -----------------------------------------------------
            request_json <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/report/review"
            ),
            httr::add_headers(.headers = headers),
            query = params
            )

            if (request_json$status_code == 200) {
              response_json <-
                httr::content(request_json,
                              type = "application/json",
                              encoding = "UTF-8") %>%
                purrr::list_transpose() %>%
                # Rectangularize (zu tibble)
                tibble::as_tibble() %>%
                dplyr::rename(
                  content = "category: content",
                  design = "category: design",
                  tech = "category: tech",
                ) %>%
                tidyr::unnest(c(content, design, tech), keep_empty = TRUE) %>%
                dplyr::select(
                  -c("category: ")
                )
            } else {
              response_json <- tibble::tibble()
            }

            return(response_json)
          })
