#' Generate API request function
#'
#' @param base_url Character. Base URL of the instance.
#' @param auth_token Character. Token to interact with the instance API.
#' @param version Character. Version of the IQB Studio Lite (not necessary for the IQB Testcenter).
#'
#' @description
#' This function returns the base API request for a an instance.
#'
#' @return A `function` with arguments `method`, `endpoint`, and `query`.
#'
#' @examples
generate_base_req <- function(base_url, auth_token, version = NULL) {
  base_call <-
    httr2::request(base_url = base_url) %>%
    httr2::req_headers("app-version" = version,
                       Authorization = auth_token
    )

  function(method, endpoint, query) {
    base_call %>%
      httr2::req_method(method) %>%
      httr2::req_url_path_append("api", endpoint) %>%
      httr2::req_url_query(!!! query)
  }
}
