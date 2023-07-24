setGeneric("readData", function(login, workspace, groups) {
  standardGeneric("readData")
})

# ONLY WORKS FOR 1 GROUP, YET
setMethod("readData",
          signature = signature(login = "Login"),
          function(login, workspace, groups) {
            ws_id <- login@access[[workspace]]

            headers = c(
              AuthToken = login@token
            )

            params = list(
              dataIds = glue::glue_collapse(groups, sep = ",")
            )

            request <- httr::GET(url = glue::glue("{login@domain}/workspace/{ws_id}/report/response"),
                                 httr::add_headers(.headers=headers),
                                 query = params
            )

            # Einlesen als Liste
            httr::content(request) %>%
              # Rectangularize (zu tibble)
              tibble::enframe(name = NULL) %>%
              # Schleife zum Spreaden der Einträge (Auslesen in tibble)
              dplyr::mutate(
                # Ladebalken?
                value = purrr::map(value, \(x) tibble::as_tibble(x))
              ) %>%
              # Entpacken
              tidyr::unnest(value) %>%
              # Schleife zum Spreaden der Response- und LastState-Einträge (Auslesen in tibble)
              dplyr::mutate(
                responses = purrr::map(responses, \(x) x$content %>%
                                         jsonlite::parse_json(simplifyVector = TRUE) %>%
                                         tibble::as_tibble()),
                laststate = purrr::map(laststate, \(x) x %>%
                                         jsonlite::parse_json(simplifyVector = TRUE) %>%
                                         tibble::as_tibble()),
              ) %>%
              # Entpacken
              tidyr::unnest(c(
                responses,
                laststate
              ))

          })
