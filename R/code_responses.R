#' Code unit responses with coding schemes
#'
#' @param responses Tibble. Response data retrieved from the IQB Testcenter with setting the argument `prepare = FALSE` for [get_responses()] or [read_responses()].
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#' @param prepare Logical. Whether to unpack the coding results and to add information from the coding schemes.
#' @param by Character. Additional columns as subgroups for the coding (e.g., in case of duplicate unit data for a specific person that could emerge in offline settings).
#'
#' @description
#' This function automatically codes responses by using the `eatAutoCode` package.
#'
#' @return A tibble.
#' @export
code_responses <- function(responses,
                           units,
                           prepare = FALSE,
                           by = NULL) {
  cli_setting()

  coding_schemes <-
    units %>%
    dplyr::select(unit_key, coding_scheme)

  responses_prepared <-
    responses %>%
    # Helper to get rid off (for this step) unnecessary stateVariables
    dplyr::mutate(
      response_id = purrr::map_chr(responses, "id")
    ) %>%
    dplyr::filter(response_id == "elementCodes") %>%
    # TODO: Already rename on get_responses()
    dplyr::rename(
      dplyr::any_of(c(
        group_id = "groupname",
        login_name = "loginname",
        login_code = "code",
        booklet_id = "bookletname",
        unit_key = "unitname"
      ))
    ) %>%
    tidyr::nest(unit_responses = -any_of(c("unit_key", by))) %>%
    # Filter off units without coding scheme
    dplyr::semi_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::left_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::arrange(unit_key)

  if (is.null(by)) {
    progress_bar_format <- "Coding unit {cli::pb_current + 1}/{cli::pb_total}: {.unit-key {responses_prepared$unit_key[cli::pb_current+1]}}: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Successfully coded {cli::pb_total} unit{?s}"
  } else {
    progress_bar_format <- "Coding unit {.unit-key {responses_prepared$unit_key[cli::pb_current+1]}} for subgroup: {cli::pb_current + 1}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Successfully coded {cli::pb_total} subgroup{?s}."
  }

  responses_coded <-
    responses_prepared %>%
    dplyr::mutate(
      unit_codes = purrr::pmap(
        .l = list(unit_responses, coding_scheme),
        .f = code_unit,
        .progress = list(
          type ="custom",
          show_after = 0,
          format = progress_bar_format,
          format_done = progress_bar_format_done,
          clear = FALSE
        ))
    )

  if (prepare) {
    prepared_coding_schemes <-
      coding_schemes %>%
      dplyr::filter(unit_key %in% unique(responses_coded$unit_key)) %>%
      dplyr::mutate(
        prepared_coding_schemes = purrr::map(coding_scheme,
                                             function(cs) {
                                               cs %>%
                                                 prepare_coding_scheme() %>%
                                                 # ... could otherwise not be unpacked
                                                 dplyr::select(-any_of(c("page",
                                                                         "fragmenting",
                                                                         "valueArrayPos")))
                                             },
                                             .progress = "Prepare coding schemes")
      ) %>%
      tidyr::unnest(prepared_coding_schemes) %>%
      dplyr::select(unit_key, variable_id, source_type, code_id, code_type, derive_sources)

    pcs_variables <-
      prepared_coding_schemes %>%
      dplyr::distinct(unit_key, variable_id, source_type)

    pcs_codes <-
      prepared_coding_schemes %>%
      dplyr::distinct(unit_key, variable_id, code_id, code_type, derive_sources) %>%
      dplyr::filter(!is.na(code_id))

    responses_coded %>%
      dplyr::select(unit_key, unit_codes) %>%
      tidyr::unnest(unit_codes) %>%
      dplyr::select(unit_key, group_id, login_name, login_code,
                    booklet_id, codes) %>%
      tidyr::unnest(codes) %>%
      dplyr::rename(variable_id = id, code_id = code) %>%
      tidyr::unnest(value, keep_empty = TRUE) %>%
      dplyr::left_join(pcs_variables, by = dplyr::join_by("unit_key", "variable_id")) %>%
      dplyr::left_join(pcs_codes, by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
      dplyr::select(
        unit_key,
        variable_id,
        source_type,
        group_id,
        login_code,
        value,
        status,
        code_id,
        code_type,
        score)
  } else {
    return(responses_coded)
  }
}

#' Code the responses of one unit responses with its coding scheme
#'
#' @param unit_responses Character. Response data of one unit retrieved from the IQB Testcenter in JSON format.
#' @param coding_scheme Character. Coding scheme of the unit retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#'
#' @description
#' This function automatically codes responses of one unit by using the `eatAutoCode` package.
#'
#' @return A tibble.
#'
#' @keywords internal
code_unit <- function(unit_responses, coding_scheme) {
  unit_responses %>%
    dplyr::mutate(
      responses = purrr::map_chr(responses, "content")
    ) %>%
    dplyr::filter(responses != "[]") %>%
    dplyr::mutate(
      codes = purrr::map(responses, function(resp) {
        eatAutoCode::code_responses(coding_scheme = coding_scheme,
                                    responses = resp) %>%
          dplyr::mutate(
            value = purrr::map(value, function(x) {
              if (length(x) > 1) {
                x %>% stringr::str_c(collapse = ",")
              } else {
                as.character(x)
              }
            })
          )
      })
    )
}
