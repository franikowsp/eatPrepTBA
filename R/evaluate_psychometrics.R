#' Evaluates frequencies and discrimination parameters of codes and categories
#'
#' @param coded Tibble. Response data coded with [code_responses()]. The argument `prepare` must be `TRUE`.
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `metadata = TRUE` for [get_units()] -- otherwise the item values could only be inferred from the variable source tree, i.e., item scores are taken from variable scores that are no source variables for other derived variables. Could optionally also contain `unit_codes` prepared by `add_coding_scheme()` (saves some time).
#' @param domains Tibble. Contains columns `domain` and `unit_key`. Currently, the routine only works for one-dimensional `domain`, i.e., there is only one `domain` for each `unit_key`. If not specified, the `workspace_label` is regarded as the unit domain.
#' @param max_n_categories Tibble. Maximum number of categories to check for category frequencies for list values, e.g., `[[01_1,01_2]]`. Defaults to `10`.
#' @param overwrite Logical. Should column `unit_codes` be overwritten if they exist on `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they were added to `units` beforehand by applying `add_coding_schemes()`.
#' @param identifiers Character. Contains person identifiers of the dataset `coded`. Defaults to `c("group_id", "login_name", "login_code")` which corresponds to the identifiers of the IQB Testcenter.
#'
#' @details
#' This function estimates item, code and category frequencies for a set of coded responses.
#'
#' @return A tibble.
#' @export
evaluate_psychometrics <- function(
    coded,
    units,
    domains = NULL,
    max_n_categories = 10,
    overwrite = FALSE,
    identifiers = c("group_id", "login_name", "login_code")
) {
  # TODO:
  # - Missing routine and design completion should come first!
  # - Double entries per person would also provide problems here as the correlation could not be estimated in that case! (double cases should be removed)

  cli_setting()
  # responses <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/data/responses.RData")
  # units <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/db/units.RData")
  # coded <- code_responses(responses, units_cs, prepare = TRUE)

  units_cs <-
    add_coding_scheme(
      units = units,
      overwrite = overwrite,
      filter_has_codes = TRUE
    )

  units_final_variables <-
    units_cs %>%
    dplyr::select(unit_key, unit_codes) %>%
    tidyr::unnest(unit_codes) %>%
    tidyr::unnest(variable_sources) %>%
    tidyr::unnest(variable_codes) %>%
    # Eliminate NO_CODING
    dplyr::filter(!is.na(code_id)) %>%
    dplyr::distinct(unit_key, variable_id, variable_source_type, variable_source_id) %>%
    dplyr::group_by(unit_key) %>%
    dplyr::filter(
      ! (variable_id %in% variable_source_id)
    ) %>%
    dplyr::distinct(unit_key, variable_id) %>%
    dplyr::arrange(unit_key, variable_id) %>%
    dplyr::mutate(item_id = seq_along(variable_id) %>%
                    stringr::str_pad(width = 2, side = "left", pad = "0")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      item_id_source = "coding_scheme"
    )

  if (tibble::has_name(units_cs, "items_list")) {
    units_items <-
      units_cs %>%
      dplyr::select(unit_key, items_list) %>%
      tidyr::unnest(items_list) %>%
      dplyr::select(unit_key, variable_id, item_id) %>%
      dplyr::filter(!is.na(item_id)) %>%
      dplyr::mutate(
        item_source = "metadata"
      )

    check_items <-
      units_items %>%
      dplyr::group_by(unit_key, variable_id) %>%
      dplyr::mutate(
        n_variable = length(variable_id)
      ) %>%
      dplyr::filter(n_variable > 1) %>%
      dplyr::ungroup()

    if (nrow(check_items) > 0) {
      cli::cli_alert_danger("Found metadata, but the following {.unit-key units} contained item-variable links that were not unique: {.unit-key {unique(check_items$unit_key)}}")
      # TODO: Ggf. nochmal anpassen, dass nur die Zweifelsfälle entsprechend verarbeitet werden
      cli::cli_alert_info("Please check. For the following analyses, the final derived variables due to the coding schemes were used insted.")

      units_items <-
        units_final_variables
    }
  }


  # TODO: variable_multiple contains a bug (should become FALSE if only one value is allowed)
  # variable_multiples <-
  #   units_cs_unnest %>%
  #   dplyr::distinct(
  #     unit_key, variable_id, variable_multiple
  #   )

  # Empirically identify multiple-value variables
  variables_multiple <-
    coded %>%
    dplyr::group_by(unit_key, variable_id) %>%
    dplyr::summarise(category_is_list = any(stringr::str_detect(value, "^\\[\\[.+\\]\\]$"), na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup()

  # Reconstruct variable labels
  variable_labels <-
    units_cs %>%
    dplyr::select(-c(coding_scheme, unit_codes)) %>%
    tidyr::unnest(unit_variables) %>%
    tidyr::unnest(variable_values) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(
      unit_key, variable_id, value, value_label
    ) %>%
    dplyr::mutate(
      # Could be a picture (add later; requires unit definition)
      value_label = ifelse(value_label == "", stringr::str_glue("Placeholder Medium {value}"), value_label)
    )

  # Frequencies

  # Code frequencies
  code_frequencies <-
    coded %>%
    dplyr::count(unit_key, variable_source_type, variable_id, code_id, code_score, code_type, code_status,
                 name = "code_n") %>%
    dplyr::group_by(unit_key, variable_source_type, variable_id) %>%
    dplyr::mutate(
      code_n_total = sum(code_n),
      # TODO: Add a column with information "valid" vs. "missing" in the missing routine
      code_n_valid = sum(code_n * !is.na(code_score)),
      code_p_total = code_n / code_n_total,
      code_p_valid = ifelse(!is.na(code_score), code_n / code_n_valid, NA)
    ) %>%
    dplyr::ungroup()

  # Category frequencies
  category_frequencies <-
    coded %>%
    dplyr::semi_join(variable_labels, by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::filter(!is.na(value)) %>%

    # TODO: Consistent missing filter!
    dplyr::filter(code_id >= 0) %>%

    dplyr::count(unit_key, variable_source_type, variable_id, code_id, code_score, code_type, code_status, value,
                 name = "category_n") %>%
    dplyr::group_by(unit_key, variable_source_type, variable_id) %>%
    dplyr::mutate(
      category_n_categories_valid = length(!is.na(code_score))
    ) %>%
    dplyr::left_join(variables_multiple, by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::filter(! category_is_list | (category_is_list & category_n_categories_valid <= max_n_categories)) %>%
    dplyr::mutate(
      # Necessary for nesting and unnesting
      value_id = seq_along(value),
      value = purrr::map(value, function(value) {
        if (is.na(value)) {
          return(value)
        }

        if (value %>% stringr::str_detect("^\\[\\[.+\\]\\]$")) {
          value %>% stringr::str_extract("^\\[\\[(.+)\\]\\]$", group = TRUE) %>%
            stringr::str_split_1(",")
        } else {
          value
        }
      })
    ) %>%
    tidyr::unnest(value) %>%
    dplyr::left_join(variable_labels, by = dplyr::join_by("unit_key", "variable_id", "value")) %>%
    tidyr::nest(category_id = value, category_label = value_label) %>%
    dplyr::select(-c(value_id, category_n_categories_valid)) %>%
    dplyr::mutate(
      dplyr::across(c("category_id", "category_label"),
                    concatenate_character)
    ) %>%
    dplyr::ungroup()

  # "Lost frequencies": Some categories might not have been used
  lost_frequency_responses <-
    variable_labels %>%
    dplyr::semi_join(category_frequencies %>% dplyr::distinct(unit_key, variable_id),
                     by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::anti_join(category_frequencies %>% dplyr::distinct(unit_key, variable_id, value = category_id, value_label = category_label),
                     by = dplyr::join_by("unit_key", "variable_id", "value", "value_label")) %>%
    dplyr::semi_join(variables_multiple %>% dplyr::filter(! category_is_list), by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::group_by(unit_key, variable_id) %>%
    dplyr::mutate(
      code_chunk = seq_along(variable_id),
      status = "VALUE_CHANGED"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value_label) %>%
    dplyr::rename(id = variable_id) %>%
    tidyr::nest(
      responses = c(id, status, value)
    )

  if (nrow(lost_frequency_responses) > 0) {

    cli::cli_alert_info("Adding missing category codes")
    lost_frequency_coding <-
      code_responses(
        responses = lost_frequency_responses,
        units = units_cs,
        prepare = TRUE,
        overwrite = FALSE
      )

    lost_category_frequencies <-
      lost_frequency_coding %>%
      dplyr::select(-code_chunk) %>%
      dplyr::filter(code_status != "UNSET",
                    variable_source_type == "BASE") %>%
      dplyr::left_join(variable_labels, by = dplyr::join_by("unit_key", "variable_id", "value")) %>%
      dplyr::rename(
        category_id = value,
        category_label = value_label
      ) %>%
      dplyr::mutate(
        category_n = 0L,
        category_is_list = FALSE
      )
  } else {
    lost_category_frequencies <- tibble::tibble()
  }

  # Merge categories
  all_category_frequencies <-
    category_frequencies %>%
    dplyr::bind_rows(lost_category_frequencies) %>%
    dplyr::arrange(unit_key, variable_id, category_id, code_id)

  # "Lost codes": Some codes might not have been used
  lost_code_frequencies <-
    units_cs %>%
    dplyr::select(unit_key, unit_codes) %>%
    tidyr::unnest(unit_codes) %>%
    tidyr::unnest(variable_codes) %>%
    dplyr::select(
      unit_key, variable_source_type, variable_id, code_id, code_score, code_type
    ) %>%
    dplyr::semi_join(code_frequencies %>% dplyr::distinct(unit_key, variable_id),
                     by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::anti_join(code_frequencies %>% dplyr::distinct(unit_key, variable_id, code_id),
                     by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
    dplyr::mutate(
      code_n = 0L,
      code_n_total = 0L,
      code_n_valid = 0L,
      code_p_total = 0,
      code_p_valid = 0,
      code_status = "CODING_COMPLETE"
    )

  # Merge categories and finalize item scores
  all_code_frequencies <-
    code_frequencies %>%
    dplyr::bind_rows(lost_code_frequencies) %>%
    dplyr::arrange(unit_key, variable_id, code_id) #%>%
  # dplyr::left_join(units_items, by = dplyr::join_by("unit_key", "variable_id"))
  #

  frequencies <-
    all_code_frequencies %>%
    dplyr::left_join(all_category_frequencies,
                     by = dplyr::join_by("unit_key", "variable_source_type", "variable_id",
                                         "code_id", "code_score", "code_type", "code_status")) %>%
    dplyr::mutate(
      # category_n_total = sum(category_n),
      # category_n_valid = sum(category_n * !is.na(code_score)),
      category_p_total = category_n / code_n_total,
      category_p_valid = category_n / code_n_valid,
    )

  # Discrimination
  domains_ws <-
    units_cs %>%
    dplyr::select(unit_key, domain = ws_label)

  if (!is.null(domains)) {
    check_domains <-
      domains %>%
      dplyr::count(domain, unit_key) %>%
      dplyr::filter(n != 1)

    if (nrow(check_domains) > 0) {
      cli::cli_alert_danger("The following units {.unit-key units} were part of more than one domain which is not allowed in the current version of eatPrepTBA: {.unit-key {unique(check_domains$unit_key)}}")
      # TODO: Ggf. nochmal anpassen, dass nur die Zweifelsfälle entsprechend verarbeitet werden
      cli::cli_alert_info("Please check. For the following analyses, the {.ws-label workspace label} will be regarded as the domain.")

      domains <- domains_ws
    }
  } else {
    domains <- domains_ws
  }

  # Scores
  coded_domains <-
    coded %>%
    dplyr::left_join(
      domains, by = dplyr::join_by("unit_key")
    ) %>%
    # dplyr::left_join(domains) %>%
    tidyr::unnest(domain)

  coded_domain_scores <-
    coded_domains %>%
    dplyr::left_join(units_items, by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::filter(!is.na(item_id)) %>%
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c("domain", identifiers)))
    ) %>%
    dplyr::summarise(
      domain_score = mean(code_score, na.rm = TRUE),
      # TODO: For part-whole-correction?
      # domain_score_pw = domain_score - code_score
      .groups = "drop"
    )

  # Code level
  code_discriminations <-
    coded_domains %>%
    dplyr::left_join(coded_domain_scores,
                     by = dplyr::join_by("domain", !!! identifiers)) %>%
    dplyr::select(dplyr::any_of(c("domain", identifiers,
                                  "unit_key", "variable_id", "code_id", "domain_score"))) %>%
    tidyr::nest(data = dplyr::any_of(c(identifiers, "code_id", "domain_score"))) %>%
    dplyr::mutate(
      data = purrr::map(data, function(x) category_correlation(x,
                                                               identifiers = identifiers,
                                                               input_name = "code_id",
                                                               output_name = "code_id",
                                                               output_value = "code_pbc"),
                        .progress = "Estimating code discrimination")
    ) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      code_id = suppressWarnings(as.integer(code_id))
    )

  # Category discrimination
  category_discriminations <-
    coded_domains %>%
    dplyr::semi_join(variable_labels, by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::left_join(coded_domain_scores,
                     by = dplyr::join_by("domain", !!! identifiers)) %>%
    dplyr::select(dplyr::any_of(c("domain", identifiers, "unit_key", "variable_id", "value", "domain_score"))) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(unit_key, variable_id) %>%
    dplyr::mutate(
      category_n_categories_valid = length(unique(value))
    ) %>%
    dplyr::left_join(variables_multiple, by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::filter(! category_is_list | (category_is_list & category_n_categories_valid <= max_n_categories)) %>%
    dplyr::select(-dplyr::any_of(c("category_is_list", "category_n_categories_valid"))) %>%
    tidyr::nest(data = dplyr::any_of(c(identifiers, "value", "domain_score"))) %>%
    dplyr::mutate(
      data = purrr::map(data, function(x) category_correlation(x,
                                                               identifiers = identifiers,
                                                               input_name = "value",
                                                               output_name = "category_id",
                                                               output_value = "category_pbc"),
                        .progress = "Estimating category discrimination")
    ) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      category_id = ifelse(category_id == "NA", NA_character_, category_id)
    ) %>%
    dplyr::ungroup()

  frequencies %>%
    dplyr::left_join(code_discriminations, by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
    dplyr::left_join(category_discriminations, by = dplyr::join_by("unit_key", "variable_id", "category_id", "domain")) %>%
    dplyr::select(
      dplyr::any_of(c(
        "unit_key",
        "variable_id", "variable_source_type",
        "code_id", "code_type", "code_score", "code_status",
        "code_n", "code_n_total", "code_n_valid", "code_p_total", "code_p_valid",
        "domain", "code_pbc",
        "category_id", "category_label", "catagory_is_list",
        "category_n", "category_p_total", "category_p_valid",
        "category_pbc"
      ))
    ) #%>%
    # readr::write_rds("Q:/BiStaTest/Primar/2_Pilotierung/51b_Einlesekontrolle/data/new-category.RData")
}

concatenate_character <- function(value) {
  purrr::map_chr(value, function(x) {
    x_prep <- x %>% tibble::deframe()

    if (length(x_prep) == 1) {
      as.character(x_prep)
    } else {
      list_vals <- stringr::str_c(x_prep, collapse = ",")
      stringr::str_glue("[[{list_vals}]]")
    }
  })
}

category_correlation <- function(data, identifiers, input_name = "code_id", output_name = "code_id", output_value = "code_pbc") {
  data %>%
    dplyr::mutate(
      code_dummy = 1
    ) %>%
    tidyr::pivot_wider(names_from = input_name, values_from = "code_dummy", values_fill = 0) %>%
    dplyr::summarise(
      dplyr::across(-dplyr::any_of(c(identifiers, "domain_score")),
                    function(x) suppressWarnings(cor(x, domain_score, use = "complete.obs")))
    ) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = output_name, values_to = output_value)
}
