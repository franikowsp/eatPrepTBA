prepare_metadata <- function(unit, resp_metadata, workspace) {
  str_replacements <- "[-:/ ]+"

  # Unit metadata
  if (!is.null(resp_metadata$metadata$profiles)) {
    unit_profiles_prep <-
      resp_metadata$metadata$profiles %>%
      purrr::keep("isCurrent") %>%
      purrr::pluck(1, "entries") %>%
      purrr::map(function(x) {
        unlist(x) %>%
          as.list() %>%
          tibble::enframe() %>%
          tidyr::unnest(value) %>%
          tidyr::pivot_wider(values_fn = function(x) stringr::str_c(x, collapse = ";"))
      }) %>%
      purrr::reduce(dplyr::bind_rows)

    value_id_exists <- tibble::has_name(unit_profiles_prep, "value.id")
    value_text_exists <- tibble::has_name(unit_profiles_prep, "valueAsText.value")

    unit_profiles <-
      unit_profiles_prep %>%
      dplyr::mutate(
        value.id = if (value_id_exists) value.id else NA,
        valueAsText.value = if (value_text_exists) valueAsText.value else NA,
        dplyr::across(c(value.id, valueAsText.value), function(x) stringr::str_split(x, ";")),
        profile_name = stringr::str_replace_all(label.value, str_replacements, "_")
      ) %>%
      tidyr::unnest(c(value.id, valueAsText.value)) %>%
      dplyr::select(
        profile_name,
        value_id = value.id,
        value_text = valueAsText.value
      )
  } else {
    unit_profiles <-
      tibble::tibble(
        profile_name = NA,
        value_id = NA,
        value_text = NA
      )
  }

  # Item metadata
  if (length(resp_metadata$metadata$items) != 0) {
    items_meta <-
      resp_metadata$metadata$items %>%
      purrr::map(function(x) {
        purrr::discard(x, .p = names(x) == "profiles") %>%
          purrr::map(function(x) if (is.null(x)) NA else x) %>%
          tibble::as_tibble()
      }) %>%
      tibble::enframe(name = "item") %>%
      tidyr::unnest(value) %>%
      dplyr::rename(any_of(c(
        item_no = "item",
        item_id = "id",
        variable_id = "variableId"
      )))

    if (tibble::has_name(items_meta, "description")) {
      items_meta <-
        items_meta %>%
        dplyr::rename(
          item_description = description
        )
    }
  } else {
    items_meta <-
      tibble::tibble(
        item_id = NA_character_,
        variable_id = NA_character_,
        profile_name = "Itemformat",
        value_id = NA_character_,
        value_text = ""
      )
  }

  # !is.null(unit$metadata$items %>% purrr::map("profiles") %>% purrr::reduce(c)) &&

  if (length(resp_metadata$metadata$items) != 0) {
    items_profiles <-
      resp_metadata$metadata$items %>%
      purrr::map(function(x) {
        x %>%
          purrr::pluck("profiles") %>%
          purrr::keep("isCurrent")
      }) %>%
      purrr::map(function(x) {
        entries <- x %>%
          purrr::pluck(1, "entries") %>%
          purrr::map(function(x) {
            unlist(x) %>%
              as.list() %>%
              tibble::enframe() %>%
              tidyr::unnest(value) %>%
              tidyr::pivot_wider(values_fn = function(x) stringr::str_c(x, collapse = "_-_-_"))
          }) %>%
          purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())}) %>%
      tibble::enframe(name = "item") %>%
      tidyr::unnest(value) %>%
      dplyr::select(item, label.value, value.id, valueAsText.value) %>%
      dplyr::mutate(
        dplyr::across(c(value.id, valueAsText.value),
                      function(x) stringr::str_split(x, "_-_-_"))
      ) %>%
      tidyr::unnest(c(value.id, valueAsText.value)) %>%
      dplyr::mutate(
        profile_name = stringr::str_replace_all(label.value,
                                                str_replacements,
                                                "_"),
      ) %>%
      dplyr::select(
        item_no = item,
        profile_name,
        # profile_label = label.value,
        value_id = value.id,
        value_text = valueAsText.value
      )
  } else {
    items_profiles <-
      tibble::tibble(
        item_no = NA_integer_,
        profile_name = NA_character_,
        # profile_label = label.value,
        value_id = NA_character_,
        value_text = ""
      )
  }

  # TODO: Add this to the global workspace object
  ws_info <-
    get_settings(workspace, metadata = FALSE) %>%
    dplyr::select(
      ws_id, ws_label, wsg_id, wsg_label
    )

  unit %>%
    dplyr::mutate(
      ws_info = ws_info
    ) %>%
    tidyr::unnest(ws_info) %>%
    dplyr::mutate(
      unit_profiles = list(unit_profiles),
      items_meta = list(items_meta),
      items_profiles = list(items_profiles),
    )
}
