evaluate_frequency <- function() {
  # responses <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/data/responses.RData")
  # units <- readr::read_rds("Q:/BiStaTest/SekI_Sprachen/2_Pilotierung/50_Datenaufbereitung/db/units.RData")
  #
  # coded_responses <- code_responses(responses, units, prepare = TRUE)
  #
  # coded_responses %>% View
  #
  # variable_labels <-
  #   units %>%
  #   tidyr::unnest(unit_variables) %>%
  #   tidyr::unnest(variable_values) %>%
  #   dplyr::filter(!is.na(value)) %>%
  #   dplyr::select(
  #     unit_key, variable_id, value, value_label
  #   )
  #
  # coding_schemes_prepared$variable_codes
  #
  # units %>%
  #   dplyr::filter(unit_key == "GS_ChK16") %>% tidyr::unnest(unit_variables) %>%
  #   dplyr::filter(variable_id == "01a")
  #
  # coded_responses %>%
  #   dplyr::filter(unit_key == "GS_ChK16", variable_id == "01a") %>%
  #   dplyr::count(unit_key, variable_source_type, variable_id, code_id, code_score, code_type, code_status) %>%
  #   dplyr::left_join(variable_labels)

}
