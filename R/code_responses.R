#' Code unit responses with coding schemes
#'
#' @param responses Tibble. Response data retrieved from the IQB Testcenter with setting the argument `prepare = FALSE` for [get_responses()] or [read_responses()].
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#' @param prepare Logical. Whether to unpack the coding results and to add information from the coding schemes.
#' @param by Character. Additional columns as subgroups for the coding (e.g., in case of duplicate unit data for a specific person that could emerge in offline settings).
#' @param codes_manual Tibble (optional). Data frame holding the manual codes. Defaults to `NULL` and does only automatic coding.
#' @param missings Tibble (optional). Provide missing meta data with `code_id`, `status`, `score`, and `code_type`. Defaults to `NULL` and uses default scheme.
#'
#' @description
#' This function automatically codes responses by using the `eatAutoCode` package.
#'
#' @return A tibble.
#' @export
code_responses <- function(responses,
                           units,
                           prepare = FALSE,
                           by = NULL,
                           codes_manual = NULL,
                           missings = NULL) {
  cli_setting()

  if (is.null(missings)) {
    missings <-
      tibble::tribble(
        ~code_id, ~status, ~code_score, ~code_type,
        -97, "CODING_ERROR", -97, "MISSING CODING IMPOSSIBLE",
        -98, "INVALID", -98, "MISSING INVALID RESPONSE",
        -99, "DISPLAYED", -99, "MISSING BY OMISSION"
      )
  }

  cli::cli_h3("Prepare responses")

  responses_prepared <-
    responses %>%
    # Helper to get rid of (for this step) unnecessary stateVariables
    dplyr::mutate(
      response_id = purrr::map_chr(responses, "id")
    ) %>%
    dplyr::filter(response_id == "elementCodes") %>%
    dplyr::select(-response_id) %>%
    dplyr::mutate(
      responses = purrr::map_chr(responses, "content")
    ) %>%
    dplyr::filter(responses != "[]") %>%
    # TODO: Already rename on get_responses()
    dplyr::rename(
      dplyr::any_of(c(
        group_id = "groupname",
        login_name = "loginname",
        login_code = "code",
        booklet_id = "bookletname",
        unit_key = "unitname"
      ))
    )

  coding_schemes <-
    units %>%
    dplyr::filter(
      unit_key %in% (responses_prepared$unit_key)
    ) %>%
    dplyr::select(unit_key, coding_scheme)

  # Insert manual codes
  if (!is.null(codes_manual) || prepare) {
    cli::cli_h3("Prepare coding schemes")

    coding_schemes_prepared <-
      coding_schemes %>%
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
      dplyr::select(unit_key, variable_id, source_type, code_id, code_type, code_score, derive_sources)

    pcs_variables <-
      coding_schemes_prepared %>%
      dplyr::distinct(unit_key, variable_id, source_type)

    pcs_codes <-
      coding_schemes_prepared %>%
      dplyr::distinct(unit_key, variable_id, code_id, code_type, code_score, derive_sources) %>%
      dplyr::filter(!is.na(code_id))
  }

  if (!is.null(codes_manual)) {
    pcs_variables_insert <-
      pcs_variables %>%
      dplyr::select(-source_type)

    pcs_codes_insert <-
      pcs_codes %>%
      dplyr::mutate(status = "CODING_COMPLETE") %>%
      dplyr::select(-derive_sources, -code_type)

    codes_manual_prepared <-
      codes_manual %>%
      dplyr::select(any_of(c(
        group_id = "groupId",
        booklet_id = "bookletId",
        login_code = "personId",
        variable_id = "variableId",
        unit_key = "unitId",
        code_id = "code"
      ))) %>%
      dplyr::mutate(
        code_id = as.integer(code_id)
      ) %>%
      dplyr::left_join(pcs_variables_insert, by = dplyr::join_by("unit_key", "variable_id")) %>%
      dplyr::left_join(pcs_codes_insert, by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
      dplyr::left_join(missings %>% dplyr::select(-code_type), by = dplyr::join_by("code_id"), suffix = c("", "_miss")) %>%
      dplyr::mutate(
        status = dplyr::coalesce(status, status_miss),
        code_score = dplyr::coalesce(code_score, code_score_miss)
      ) %>%
      dplyr::select(-status_miss, -code_score_miss)

    codes_to_merge <-
      codes_manual_prepared %>%
      dplyr::rename(id = variable_id, code = code_id, score = code_score) %>%
      dplyr::mutate(
        value = code
      ) %>%
      tidyr::nest(
        codes_manual = c(id, code, score, status, value)
      ) %>%
      dplyr::mutate(
        codes_manual = purrr::map(codes_manual, function(x) {
          x %>%
            as.list() %>%
            purrr::list_transpose()
        })
      )

    responses_inserted <-
      responses_prepared %>%
      dplyr::left_join(
        codes_to_merge, by = dplyr::join_by("group_id", "login_code", "booklet_id", "unit_key")
      ) %>%
      dplyr::mutate(
        responses = purrr::map2_chr(responses, codes_manual, update_list)
      ) %>%
      dplyr::select(-codes_manual)
  } else {
    responses_inserted <- responses_prepared
  }

  responses_for_coding <-
    responses_inserted %>%
    tidyr::nest(unit_responses = -any_of(c("unit_key", by))) %>%
    # Filter off units without coding scheme
    dplyr::semi_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::left_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::arrange(unit_key)

  unit_keys <- responses_for_coding$unit_key
  n_units <- length(unique(units))

  cli::cli_h3("Start coding")

  if (is.null(by)) {
    progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Successfully coded {cli::pb_total} unit{?s} in {cli::pb_elapsed}."
  } else {
    progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} for subgroup: {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

    progress_bar_format_done <- "Successfully coded {cli::pb_extra$n_units} unit{?s} in {cli::pb_total} subgroup{?s} in {cli::pb_elapsed}."
  }

  responses_coded <-
    responses_for_coding %>%
    dplyr::mutate(
      unit_codes = purrr::pmap(
        .l = list(unit_responses, coding_scheme),
        .f = code_unit,
        .progress = list(
          type ="custom",
          show_after = 0,
          extra = list(
            unit_keys = unit_keys,
            n_units = n_units
          ),
          format = progress_bar_format,
          format_done = progress_bar_format_done,
          clear = FALSE
        ))
    )

  if (prepare) {
    # TODO: Does not work if no code_ids are available (e.g., only coding errors)
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
    # dplyr::mutate(
    #   responses = purrr::map_chr(responses, "content")
    # ) %>%
    # dplyr::filter(responses != "[]") %>%
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

#' Add manual codes to unit responses
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
update_list <- function(unit_responses, unit_codes_manual) {
  if (!is.null(unit_codes_manual) && length(unit_codes_manual) > 0) {
    unit_responses_unpacked <-
      unit_responses %>%
      jsonlite::parse_json()

    base_ids <-
      unit_responses_unpacked %>%
      purrr::map_chr("id")

    manual_ids <-
      unit_codes_manual %>%
      purrr::map_chr("id")

    unit_responses_inserted <- unit_responses_unpacked

    # Base variable replacement
    if (any(manual_ids %in% base_ids)) {
      unit_responses_inserted <-
        unit_responses_inserted %>%
        purrr::map(function(old_item) {
          # Check if there is a matching item in codes_manual based on id
          match_item <- purrr::detect(unit_codes_manual, function(new_item) new_item$id == old_item$id)
          # If there's a match, use it; otherwise, keep the original item from list_a
          if (!is.null(match_item)) {
            purrr::list_modify(old_item, !!! match_item)
          } else {
            old_item
          }
        })
    }

    # Derived variable addition
    if (any(! manual_ids %in% base_ids)) {
      additions <-
        unit_codes_manual %>%
        purrr::keep(function(x) ! x$id %in% base_ids)

      unit_responses_inserted <-
        unit_responses_inserted %>%
        c(additions)

    }

    unit_responses_inserted %>%
      jsonlite::toJSON(auto_unbox = TRUE, null = "null") %>%
      as.character()

  } else {
    return(unit_responses)
  }
}
