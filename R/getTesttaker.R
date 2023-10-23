#' Get testtakers file
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve testtakers file from the API.
#' @param id Character. Name of the testtakers file to be retrieved.
#'
#' @description
#' This function only returns the testtakers information for a single testtakers file To retrieve multiple testtakers files, use [getTesttakers()].
#'
#' @return A tibble.
#'
#'
#' @examples
#' @aliases
#' getTesttaker,Workspace-method
setGeneric("getTesttaker", function(workspace, id) {
  standardGeneric("getTesttaker")
})

#' @describeIn getTesttaker Get testtaker information in a in a defined workspace
setMethod("getTesttaker",
          signature = signature(workspace = "Workspace"),
          function(workspace, id) {
            domain <- workspace@login@domain
            ws_id <- workspace@id

            headers = c(
              AuthToken = workspace@login@token
            )

            # Read unit XML -----------------------------------------------------------
            # Item data and meta data
            request_xml <- httr::GET(url = glue::glue(
              "{domain}/workspace/{ws_id}/file/Testtakers/{id}.xml"
            ),
            config = httr::add_headers(.headers=headers)
            )

            if (request_xml$status_code == 200) {
              response_xml <-
                httr::content(request_xml,
                              type = "application/xml",
                              encoding = "UTF-8") %>%
                xml2::as_list() %>%
                purrr::pluck("Testtakers") %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(values_fn = list)
            } else {
              response_xml <- tibble::tibble()
            }

            # Transformations
            response_tbl <-
              response_xml %>%
              dplyr::select(
                Group
              ) %>%
              dplyr::mutate(
                Group = .extractGroup(Group)
              ) %>%
              tidyr::unnest(
                Group
              )

            return(response_tbl)
          })

# Helpers
.extractGroup <- function(Group) {
  login_information <-
    Group %>%
    purrr::map_depth(.depth = 3, .f = function(x) {
      Booklet <- x[["Booklet"]][[1]]
      attributes(Booklet) <- NULL
      Booklet

      attributes(x) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          booklet = Booklet
        ) %>%
        dplyr::distinct()
    }) %>%
    purrr::map_depth(.depth = 2, .f = function(x) dplyr::bind_rows(x)) %>%
    purrr::map_depth(.depth = 1, .f = function(x) dplyr::bind_rows(x)) %>%
    # Avoid naming conflicts
    purrr::map(.f = function(x) {
      x %>%
        dplyr::select(
          mode,
          login_name = name,
          booklet,
        )
    })

  group_names <-
    Group %>%
    purrr::map_depth(.depth = 2, .f = function(x) attributes(x) %>% tibble::as_tibble()) %>%
    purrr::map_depth(.depth = 1, .f = function(x) dplyr::bind_rows(x)) %>%
    # Avoid naming conflicts
    purrr::map(.f = function(x) {
      x %>%
        dplyr::select(
          groupname = id,
          group_label = label
        )
    })

  group_names %>%
    purrr::imap(
      .f = function(x, index) {
        dplyr::bind_cols(
          x,
          login_information[[index]],
        )
      }
    )

}
