#' Adds prepared coding scheme to units
#'
#' @param units Tibble holding units retrieved from [get_units()].
#' @param filter_has_codes Only returns variables that were not deactivated. Defaults to `TRUE`.
#'
#' @description
#' This function is called within [add_coding_scheme()] to extend the `variable_sources`. The function can also propose variable pages if [get_units()] was called with `unit_definition = TRUE`.
#'
#' @return A tibble.
#' @export
add_coding_scheme <- function(units, filter_has_codes = TRUE) {
  cli_setting()

  unit_keys <- units$unit_key

  if (!tibble::has_name(units, "coding_scheme")) {
    cli::cli_abort("No column {.field coding_scheme} in {.unit-key units}.")
  }

  if (length(unit_keys) > 0) {
    units_coding <-
      units %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "ws_id",
            "ws_label",
            "unit_id",
            "unit_label",
            "unit_key"
          )
        ),
        dplyr::any_of(
          c(
            "schemer",
            "scheme_type",
            "last_change_scheme"
          )
        ),
        dplyr::all_of(
          c(
            "coding_scheme",
            "unit_variables"
          )
        )
      ) %>%
      # dplyr::slice(9) %>%  #%>% .$coding_scheme %>% .[[1]] -> coding_scheme
      dplyr::mutate(
        coding_scheme = purrr::imap(coding_scheme, function(coding_scheme, i) {
          # print(i)
          prepare_coding_scheme(coding_scheme, filter_has_codes = filter_has_codes)
        },
        .progress = list(
          type ="custom",
          extra = list(
            unit_keys = unit_keys
          ),
          format = "Preparing coding scheme for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
          format_done = "Prepared {cli::pb_total} coding scheme{?s} in {cli::pb_elapsed}.",
          clear = FALSE
        ))
      ) %>%
      tidyr::unnest(coding_scheme) %>%
      tidyr::nest(
        variable_codes = dplyr::any_of(c(
          "code_id", "code_type", "code_label", "code_score",
          "code_manual_instruction",
          "rule_set_no", "rule_set_operator", "rule_set_array_position",
          "rule_operator", "rule_fragment_position", "rule_method", "rule_parameter"))
      )

    # Derive sources from coding scheme
    units_ds <-
      units_coding %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "ws_id",
            "unit_id",
            "unit_key",
            "variable_ref",
            variable_source_ref = "derive_sources"
          )
        )
      ) %>%
      tidyr::unnest(variable_source_ref) %>%
      tidyr::unnest(variable_source_ref) %>%
      dplyr::mutate(
        variable_source_direct = TRUE
      )

    units_st <-
      units_coding %>%
      add_source_tree() %>%
      dplyr::left_join(
        units_ds,
        by = dplyr::join_by("ws_id", "unit_id",
                            "unit_key", "variable_ref", "variable_source_ref")
      ) %>%
      dplyr::mutate(
        variable_source_direct = dplyr::coalesce(variable_source_direct, FALSE)
      )

    if (tibble::has_name(units, "variable_pages")) {
      units_pages <-
        units %>%
        dplyr::select(
          ws_id,
          unit_id,
          unit_key,
          variable_pages
        ) %>%
        tidyr::unnest(variable_pages)

      units_st_nest <-
        units_st %>%
        dplyr::left_join(
          units_pages,
          by = dplyr::join_by("ws_id", "unit_id", "unit_key", "variable_ref")
        ) %>%
        dplyr::left_join(
          units_pages %>%
            dplyr::rename(
              variable_source_ref = "variable_ref",
              variable_source_page = "variable_page",
              variable_source_page_always_visible = "variable_page_always_visible"),
          by = dplyr::join_by("ws_id", "unit_id", "unit_key", "variable_source_ref")
        ) %>%
        # Infer variable page
        dplyr::group_by(ws_id, unit_id, unit_key, variable_ref) %>%
        # dplyr::filter(variable_ref == "01") %>% View()
        dplyr::mutate(
          variable_page = dplyr::case_when(
            !is.na(variable_page) ~ as.character(variable_page),
            .default = variable_source_page %>% na.omit() %>% unique() %>% stringr::str_c(collapse = ",")
          ),
          # If NA, the user would have to take a closer look
          variable_page_always_visible = dplyr::case_when(
            !is.na(variable_page_always_visible) ~ variable_page_always_visible,
            # In case of no 1:1 relatiionship, this should be omitted
            all(is.na(variable_source_page_always_visible)) ~ NA,
            all(na.omit(variable_source_page_always_visible)) | all(!na.omit(variable_source_page_always_visible)) ~
              any(variable_source_page_always_visible),
            .default = NA
          ),
        ) %>%
        dplyr::ungroup() %>%
        tidyr::nest(variable_sources = dplyr::any_of(c(
          "variable_source_id", "variable_source_ref",
          "variable_source_level", "variable_source_direct",
          "variable_source_page", "variable_source_page_always_visible")))
    } else {
      units_st_nest <-
        units_st %>%
        tidyr::nest(variable_sources = dplyr::any_of(c(
          "variable_source_id", "variable_source_ref",
          "variable_source_level", "variable_source_direct",
          "variable_source_page", "variable_source_page_always_visible")))
    }

    # Add unit variables
    units_uv <-
      units %>%
      dplyr::distinct(
        ws_id,
        unit_id,
        unit_key,
        unit_variables
      ) %>%
      tidyr::unnest(unit_variables) %>%
      dplyr::select(-dplyr::any_of("variable_id"))

    if (tibble::has_name(units, "variable_pages")) {
      units_uv <-
        units_uv %>%
        dplyr::select(-variable_page)
    }

    units_coding %>%
      dplyr::select(-c(
        # Information is collapsed into the new variable_sources column
        "variable_sources", "derive_sources",
        # Information is directly added to the data frame
        "unit_variables"
      )) %>%
      dplyr::left_join(
        units_st_nest,
        by = dplyr::join_by("ws_id", "unit_id",
                            "unit_key", "variable_id", "variable_ref", "variable_level",
                            "variable_source_type")
      ) %>%
      dplyr::left_join(
        units_uv,
        by = dplyr::join_by("ws_id", "unit_id", "unit_key", "variable_ref")
      ) %>%
      dplyr::mutate(
        # Fill derived unit variables
        dplyr::across(c("variable_format"),
                      function(x) dplyr::coalesce(x, "")),
        variable_type = dplyr::coalesce(variable_type, "derived"),
      )
  } else {
    units
  }
}
#
# get_unique <- function(values) {
#   val <- unique(na.omit(values))
#   if (length(val) > 0) val else NA
# }
