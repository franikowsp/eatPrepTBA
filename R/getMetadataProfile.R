#' Get a unit with resources
#'
#' @param json JSON file for the Skohub metadata profile.
#'
#' @description
#' This function only returns the unit information and coding scheme for a single unit. To retrieve multiple units, use [getUnits()].
#'
#' @return A tibble.
#'
#' @examples
getMetadataProfile <- function(url) {
  profile <- jsonlite::read_json(url)

  profile_prepared <-
    profile %>%
    purrr::pluck("groups", 1, "entries") %>%
    purrr::map(function(x) unlist(x) %>% as.list() %>% tibble::as_tibble()) %>%
    purrr::reduce(dplyr::bind_rows)

  profile_read <-
    profile_prepared %>%
    dplyr::select(profile_id = id, name = label.value,
                  type, profile_url = parameters.url,
                  multiple = parameters.allowMultipleValues) %>%
    dplyr::mutate(
      profile = purrr::map(profile_url, prepare_profile_list)
    ) %>%
    tidyr::unnest(profile)

  # Merge notation
  if (tibble::has_name(profile_read, "notation")) {
    profile_relabel <-
      profile_read %>%
      dplyr::mutate(
        label = dplyr::case_when(
          !is.na(notation) ~ stringr::str_glue("{{{notation}}} {prefLabel.de}") %>% as.character(),
          .default = prefLabel.de
        )
      )
  } else {
    profile_relabel <-
      profile_read %>%
      dplyr::rename(
        label = prefLabel.de
      )
  }

  profile_relabel %>%
    dplyr::select(
      name,
      value.id = id,
      label,
      multiple
    ) %>%
    tidyr::nest(
      data = -c(name, multiple)
    ) %>%
    dplyr::mutate(
      data = purrr::map(data, function(x) x %>% dplyr::mutate(label = factor(label, levels = label)))
    )
}

narrow_profile_list <- function(x) {
  purrr::map(x, function(x) {
    out <-
      purrr::discard(x, names(x) == "narrower") %>%
      unlist() %>%
      tibble::enframe() %>%
      tidyr::pivot_wider()

    if ("narrower" %in% names(x)) {
      add_narrow <-
        x %>%
        purrr::pluck("narrower") %>%
        narrow_profile_list()

      out <- dplyr::bind_rows(out, add_narrow)
    }
    return(out)
  }) %>%
    purrr::reduce(dplyr::bind_rows)
}

prepare_profile_list <- function(url) {
  if (!is.na(url)) {
    httr::GET(url) %>%
      httr::content() %>%
      purrr::pluck("hasTopConcept") %>%
      narrow_profile_list()

  } else {
    tibble::tibble()
  }
}
