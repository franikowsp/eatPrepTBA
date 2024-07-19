#' Get reviews
#'
#' @param workspace [WorkspaceTestcenter-class]. IQB Testcenter workspace information necessary to retrieve reviews from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_reviews,WorkspaceTestcenter-method
setGeneric("get_reviews", function(workspace, groups = NULL) {
  standardGeneric("get_reviews")
})

#' @describeIn get_reviews Get responses of a given Testcenter workspace
setMethod("get_reviews",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "report", "review"),
                       query = list(dataIds = groups)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Reviews could not be retrieved.")

            if (!is.null(resp)) {
              # TODO: Change to new structure
              resp %>%
                purrr::list_transpose() %>%
                # Rectangularize (zu tibble)
                tibble::as_tibble() %>%
                dplyr::rename(
                  content = "category: content",
                  design = "category: design",
                  tech = "category: tech",
                ) %>%
                tidyr::unnest(c(content, design, tech),
                              keep_empty = TRUE) %>%
                dplyr::select(
                  -c("category: ")
                ) %>%
                dplyr::rename(any_of(c(
                  group_id = "groupname",
                  login_name = "loginname",
                  code = "code",
                  booklet_id = "bookletname",
                  unit_key = "unitname",
                  priority = "priority",
                  content = "content",
                  design = "design",
                  tech = "tech",
                  review_time = "review_time",
                  review_comment = "review_comment"
                )))
            } else {
              tibble::tibble()
            }
          })
