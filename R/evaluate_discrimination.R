#' #' Evaluates discrimination coefficients for codes and categories
#' #'
#' #' @param coded Tibble. Response data coded with [code_responses()]. The argument `prepare` must be `TRUE`.
#' #' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `metadata = TRUE` for [get_units()] -- otherwise the item values could only be inferred from the variable source tree, i.e., item scores are taken from variable scores that are no source variables for other derived variables. Could optionally also contain `unit_codes` prepared by `add_coding_scheme()` (saves some time).
#' #' @param max_n_category Tibble. Maximum number of categories to check for category discriminations for list values, e.g., `[[01_1,01_2]]`.
#' #' @param overwrite Logical. Should column `unit_codes` be overwritten if they exist on `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they were added to `units` beforehand by applying `add_coding_schemes()`.
#' #'
#' #' This function estimates code and category discrimination for a set of coded responses.
#' #'
#' #' @return A tibble.
#' #' @export
#' #' Evaluates frequencies of codes and categories
#' #'
#' #' @param coded Tibble. Response data coded with [code_responses()]. The argument `prepare` must be `TRUE`.
#' #' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `metadata = TRUE` for [get_units()] -- otherwise the item values could only be inferred from the variable source tree, i.e., item scores are taken from variable scores that are no source variables for other derived variables. Could optionally also contain `unit_codes` prepared by `add_coding_scheme()` (saves some time).
#' #' @param max_n_category Tibble. Maximum number of categories to check for category frequencies for list values, e.g., `[[01_1,01_2]]`.
#' #' @param overwrite Logical. Should column `unit_codes` be overwritten if they exist on `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they were added to `units` beforehand by applying `add_coding_schemes()`.
#' #'
#' #' This function estimates item, code and category frequencies for a set of coded responses.
#' #'
#' #' @return A tibble.
#' #' @export
#' evaluate_discrimination <- function(
#'     coded,
#'     units,
#'     max_n_categories = 10,
#'     overwrite = FALSE
#' ) {
#'   cli_setting()
#'
#'   # TODO: Missing routine and design completion should come first!
#'   # responses <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/data/responses.RData")
#'   # units <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/db/units.RData")
#'
#'   units_cs <-
#'     add_coding_scheme(
#'       units = units,
#'       overwrite = overwrite,
#'       filter_has_codes = TRUE
#'     )
#'
#'   if (tibble::has_name(units_cs, "items_list")) {
#'     units_items <-
#'       units_cs %>%
#'       dplyr::select(unit_key, items_list) %>%
#'       tidyr::unnest(items_list) %>%
#'       dplyr::select(unit_key, variable_id, item_id) %>%
#'       dplyr::filter(!is.na(item_id)) %>%
#'       dplyr::mutate(
#'         item_source = "metadata"
#'       )
#'   } else {
#'     units_items <-
#'       units_cs %>%
#'       dplyr::select(unit_key, unit_codes) %>%
#'       tidyr::unnest(unit_codes) %>%
#'       tidyr::unnest(variable_sources) %>%
#'       tidyr::unnest(variable_codes) %>%
#'       # Eliminate NO_CODING
#'       dplyr::filter(!is.na(code_id)) %>%
#'       dplyr::distinct(unit_key, variable_id, variable_source_type, variable_source_id) %>%
#'       dplyr::group_by(unit_key) %>%
#'       # dplyr::filter(unit_key == "EH_JB09") %>%
#'       dplyr::filter(
#'         ! (variable_id %in% variable_source_id)
#'       ) %>%
#'       dplyr::distinct(unit_key, variable_id) %>%
#'       dplyr::arrange(unit_key, variable_id) %>%
#'       dplyr::mutate(item_id = seq_along(variable_id) %>%
#'                       stringr::str_pad(width = 2, side = "left", pad = "0")) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::mutate(
#'         item_source = "coding_scheme"
#'       )
#'   }
#'
#'   # units_items %>%
#'   #   dplyr::anti_join(units_items_true %>% dplyr::select(unit_key, variable_id))
#'
#'   # coded <- code_responses(responses, units_cs, prepare = TRUE)
#'
#'   # coded %>% dplyr::filter(unit_key == "AV_GF01") %>% dplyr::count(variable_id)
#'
#'   # Empirically identify multiple-value variables
#'   variables_multiple <-
#'     coded %>%
#'     dplyr::group_by(unit_key, variable_id) %>%
#'     dplyr::summarise(category_is_list = any(stringr::str_detect(value, "^\\[\\[.+\\]\\]$"), na.rm = TRUE)) %>%
#'     dplyr::ungroup()
#'
#'   # Reconstruct variable labels
#'   variable_labels <-
#'     units_cs %>%
#'     dplyr::select(-c(coding_scheme, unit_codes)) %>%
#'     tidyr::unnest(unit_variables) %>%
#'     tidyr::unnest(variable_values) %>%
#'     dplyr::filter(!is.na(value)) %>%
#'     dplyr::select(
#'       unit_key, variable_id, value, value_label
#'     ) %>%
#'     dplyr::mutate(
#'       # Could be a picture (add later; requires unit definition)
#'       value_label = ifelse(value_label == "", stringr::str_glue("Platzhalter Medium {value}"), value_label)
#'     )
#'
#'   # TODO: variable_multiple contains a bug (should become FALSE if only one value is allowed)
#'   # variable_multiples <-
#'   #   units_cs_unnest %>%
#'   #   dplyr::distinct(
#'   #     unit_key, variable_id, variable_multiple
#'   #   )
#'
#'
#'
#'   # "Lost frequencies": Some categories might not have been used
#'   lost_frequency_responses <-
#'     variable_labels %>%
#'     dplyr::semi_join(category_frequencies %>% dplyr::distinct(unit_key, variable_id),
#'                      by = dplyr::join_by("unit_key", "variable_id")) %>%
#'     dplyr::anti_join(category_frequencies %>% dplyr::distinct(unit_key, variable_id, value = category_id, value_label = category_label),
#'                      by = dplyr::join_by("unit_key", "variable_id", "value", "value_label")) %>%
#'     dplyr::semi_join(variables_multiple %>% dplyr::filter(! category_is_list), by = dplyr::join_by("unit_key", "variable_id")) %>%
#'     dplyr::group_by(unit_key, variable_id) %>%
#'     dplyr::mutate(
#'       code_chunk = seq_along(variable_id),
#'       status = "VALUE_CHANGED"
#'     ) %>%
#'     dplyr::ungroup() %>%
#'     dplyr::select(-value_label) %>%
#'     dplyr::rename(id = variable_id) %>%
#'     tidyr::nest(
#'       responses = c(id, status, value)
#'     )
#'
#'   cli::cli_alert_info("Adding missing category codes")
#'   lost_frequency_coding <-
#'     code_responses(
#'       responses = lost_frequency_responses,
#'       units = units_cs,
#'       prepare = TRUE,
#'       overwrite = FALSE
#'     )
#'
#'   lost_category_frequencies <-
#'     lost_frequency_coding %>%
#'     dplyr::select(-code_chunk) %>%
#'     dplyr::filter(code_status != "UNSET",
#'                   variable_source_type == "BASE") %>%
#'     dplyr::left_join(variable_labels, by = dplyr::join_by("unit_key", "variable_id", "value")) %>%
#'     dplyr::rename(
#'       category_id = value,
#'       category_label = value_label
#'     ) %>%
#'     dplyr::mutate(
#'       category_n = 0L,
#'       category_n_total = 0L,
#'       category_n_valid = 0L,
#'       category_p_total = 0,
#'       category_p_valid = 0,
#'       category_is_list = FALSE
#'     )
#'
#'   # Merge categories
#'   all_category_frequencies <-
#'     category_frequencies %>%
#'     dplyr::bind_rows(lost_category_frequencies) %>%
#'     dplyr::arrange(unit_key, variable_id, category_id, code_id)
#'
#'   # "Lost codes": Some codes might not have been used
#'   lost_code_frequencies <-
#'     units_cs %>%
#'     dplyr::select(unit_key, unit_codes) %>%
#'     tidyr::unnest(unit_codes) %>%
#'     tidyr::unnest(variable_codes) %>%
#'     dplyr::select(
#'       unit_key, variable_source_type, variable_id, code_id, code_score, code_type
#'     ) %>%
#'     dplyr::semi_join(code_frequencies %>% dplyr::distinct(unit_key, variable_id),
#'                      by = dplyr::join_by("unit_key", "variable_id")) %>%
#'     dplyr::anti_join(code_frequencies %>% dplyr::distinct(unit_key, variable_id, code_id),
#'                      by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
#'     dplyr::mutate(
#'       code_n = 0L,
#'       code_n_total = 0L,
#'       code_n_valid = 0L,
#'       code_p_total = 0,
#'       code_p_valid = 0,
#'       code_status = "CODING_COMPLETE"
#'     )
#'
#'   # Merge categories and finalize item scores
#'   all_code_frequencies <-
#'     code_frequencies %>%
#'     dplyr::bind_rows(lost_code_frequencies) %>%
#'     dplyr::arrange(unit_key, variable_id, code_id) #%>%
#'   # dplyr::left_join(units_items, by = dplyr::join_by("unit_key", "variable_id"))
#' }
#'
#' concatenate_character <- function(value) {
#'   purrr::map_chr(value, function(x) {
#'     x_prep <- x %>% tibble::deframe()
#'
#'     if (length(x_prep) == 1) {
#'       as.character(x_prep)
#'     } else {
#'       list_vals <- stringr::str_c(x_prep, collapse = ";;")
#'       stringr::str_glue("[[{list_vals}]]")
#'     }
#'   })
#' }
#'
