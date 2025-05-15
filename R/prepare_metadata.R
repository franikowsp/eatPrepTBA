str_replacements <- "[-:/ ]+"
str_removals <- "\\(\\)"

prepare_metadata <- function(unit_metadata) {
  # Unit metadata
  unit_profiles <- read_unit_profiles(unit_metadata)
  items_profiles <- read_items_profiles(unit_metadata)
  items_list <- read_items_list(unit_metadata)

  tibble::tibble(
    unit_profiles = list(unit_profiles),
    items_list = list(items_list),
    items_profiles = list(items_profiles),
  )
}

read_unit_profiles <- function(unit_metadata) {
  if (!is.null(unit_metadata$profiles) && length(unit_metadata$profiles) > 0) {
    unit_profiles_prep <-
      unit_metadata$profiles %>%
      purrr::keep("isCurrent") %>%
      purrr::pluck(1, "entries") %>%
      purrr::map(function(x) {
        unlist(x) %>%
          as.list() %>%
          tibble::enframe() %>%
          tidyr::unnest(value) %>%
          # This must be unique
          tidyr::pivot_wider(values_fn = function(x) stringr::str_c(x, collapse = ";.;.;"))
      }) %>%
      purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

    value_id_exists <- tibble::has_name(unit_profiles_prep, "value.id")
    value_text_exists <- tibble::has_name(unit_profiles_prep, "valueAsText.value")

    unit_profiles <-
      unit_profiles_prep %>%
      dplyr::mutate(
        value.id = if (value_id_exists) value.id else NA,
        valueAsText.value = if (value_text_exists) valueAsText.value else NA,
        dplyr::across(dplyr::any_of(c("value.id", "valueAsText.value")),
                      function(x) stringr::str_split(x, ";.;.;")),
        dplyr::across(dplyr::any_of(c("label.value")),
                      function(x) stringr::str_replace_all(x, str_replacements, "_") %>%
                        stringr::str_remove_all(str_removals))
      ) %>%
      tidyr::unnest(c(value.id, valueAsText.value)) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            profile_name = "label.value",
            value_id = "value.id",
            value_text = "valueAsText.value"
          )
        )
      )
  } else {
    unit_profiles <-
      tibble::tibble(
        profile_name = NA,
        value_id = NA,
        value_text = NA
      )
  }

  return(unit_profiles)
}

read_items_profiles <- function(unit_metadata) {
  if (!is.null(unit_metadata$items) && length(purrr::compact(unit_metadata$items)) != 0) {
    items_profiles <-
      unit_metadata$items %>%
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
      dplyr::select(dplyr::any_of(c("item", "label.value", "value.id", "valueAsText.value"))) %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("value.id", "valueAsText.value")),
                      function(x) stringr::str_split(x, "_-_-_"))
      ) %>%
      tidyr::unnest(dplyr::any_of(c("value.id", "valueAsText.value"))) %>%
      dplyr::mutate(
        profile_name = stringr::str_replace_all(label.value,
                                                str_replacements,
                                                "_"),
      ) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            item_no = "item",
            profile_name = "profile_name",
            # profile_label = label.value,
            value_id = "value.id",
            value_text = "valueAsText.value"
          )
        )
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

  return(items_profiles)
}

read_items_list <- function(unit_metadata) {
  if (!is.null(unit_metadata$items) && length(purrr::compact(unit_metadata$items)) != 0) {
    items_list <-
      unit_metadata$items %>%
      purrr::map(function(x) {
        purrr::discard(x, .p = names(x) == "profiles") %>%
          purrr::map(function(x) if (is.null(x)) NA else x) %>%
          tibble::as_tibble()
      }) %>%
      tibble::enframe(name = "item") %>%
      tidyr::unnest(value) %>%
      dplyr::select(any_of(c(
        item_uuid = "uuid",
        item_no = "item",
        item_id = "id",
        variable_id = "variableId",
        variable_ref = "variableReadOnlyId",
        item_order = "order",
        item_locked = "locked",
        item_position = "position",
        item_weighting = "weighting",
        item_created = "createdAt",
        item_changed = "changedAt"
      )))

    if (tibble::has_name(items_list, "description")) {
      items_list <-
        items_list %>%
        dplyr::rename(
          item_description = description
        )
    }

    if (nrow(items_list) == 0) {
      items_list <-
        tibble::tibble(
          item_id = NA_character_,
          variable_id = NA_character_
        )
    }
  } else {
    items_list <-
      tibble::tibble(
        item_id = NA_character_,
        variable_id = NA_character_
      )
  }

  return(items_list)
}
