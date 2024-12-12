#' Code unit responses with coding schemes
#'
#' @param responses Tibble. Response data retrieved from the IQB Testcenter with setting the argument `prepare = FALSE` for [get_responses()] or [read_responses()].
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `coding_scheme = TRUE` for [get_units()].
#' @param prepare Logical. Whether to unpack the coding results and to add information from the coding schemes.
#' @param codes_manual Tibble (optional). Data frame holding the manual codes. Defaults to `NULL` and does only automatic coding.
#' @param missings Tibble (optional). Provide missing meta data with `code_id`, `status`, `score`, and `code_type`. Defaults to `NULL` and uses default scheme.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function automatically codes responses by using the `eatAutoCode` package. It is already prepared for the new data format of the responses received from the [get_responses()] and [read_responses()] routines. This function will soon be deleted and be part of [code_responses()].
#'
#' @return A tibble.
#' @export
code_responses <- function(responses,
                           units,
                           prepare = FALSE,
                           codes_manual = NULL,
                           missings = NULL
) {
  cli_setting()

  if (is.null(missings)) {
    missings <-
      tibble::tribble(
        ~code_id, ~status, ~code_score, ~code_type,
        -96, "NOT_REACHED", 0, "MISSING NOT REACHED",
        -97, "CODING_ERROR", 0, "MISSING CODING IMPOSSIBLE",
        -98, "INVALID", 0, "MISSING INVALID RESPONSE",
        -99, "DISPLAYED", 0, "MISSING BY OMISSION"
      )
  }

  cli::cli_h2("Automatic coding routine")

  start_time <- Sys.time()

  cli::cli_text("Time started: {start_time}")

  cli::cli_h3("Prepare responses")

  coding_schemes <-
    units %>%
    dplyr::filter(
      unit_key %in% responses$unit_key
    ) %>%
    dplyr::select(unit_key, coding_scheme)

  unit_keys <- coding_schemes$unit_key
  n_units <- length(unique(unit_keys))

  cli::cli_text("Identified {n_units} units that can be coded.")

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
                                                                         "valueArrayPos",
                                                                         # TODO: Must urgently be reactivated and integrated!!
                                                                         "variable_alias")))
                                             },
                                             .progress = list(
                                               type ="custom",
                                               show_after = 0,
                                               extra = list(
                                                 unit_keys = unit_keys
                                               ),
                                               format = "Preparing {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                               format_done = "Prepared {cli::pb_total} coding scheme{?s} in {cli::pb_elapsed}.",
                                               clear = FALSE
                                             ))
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
    cli::cli_h3("Prepare manual codes")

    # pcs_variables_insert <-
    #   pcs_variables %>%
    #   dplyr::select(-source_type)

    pcs_codes_insert <-
      pcs_codes %>%
      dplyr::mutate(status = "CODING_COMPLETE") %>%
      dplyr::select(-derive_sources, -code_type)

    codes_manual_prepared <-
      codes_manual %>%
      dplyr::filter(unit_key %in% unit_keys) %>%
      dplyr::mutate(
        code_id = as.integer(code_id)
      ) %>%
      dplyr::left_join(
        pcs_codes_insert,
        by = dplyr::join_by("unit_key", "variable_id", "code_id")
      ) %>%
      dplyr::left_join(
        missings %>% dplyr::select(-code_type),
        by = dplyr::join_by("code_id"), suffix = c("", "_miss")
      ) %>%
      dplyr::mutate(
        status = dplyr::coalesce(status, status_miss),
        code_score = dplyr::coalesce(code_score, code_score_miss)
      ) %>%
      dplyr::select(-status_miss, -code_score_miss)

    manual_unit_keys <- codes_manual_nested$unit_key

    codes_manual_nested <-
      codes_manual_prepared %>%
      dplyr::rename(id = variable_id, code = code_id, score = code_score) %>%
      tidyr::nest(
        manual = c(id, code, score, status)
      ) %>%
      dplyr::mutate(
        manual = purrr::map_chr(
          manual,
          function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")),
          .progress = list(
            type ="custom",
            extra = list(
              unit_keys = manual_unit_keys
            ),
            format = "Preparing manual code cases for {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
            format_done = "Prepared {cli::pb_total} manual code case{?s} in {cli::pb_elapsed}.",
            clear = FALSE
          ))
      ) %>%
      dplyr::arrange(unit_key)

    # codes_manual_nested$codes_manual[[1]]
    # manual_unit_keys <- codes_manual_nested$unit_key

    # TODO: Add filter for unit_keys that are only in coding_schemes, also needs to be arranged
    responses_inserted <-
      responses %>%
      dplyr::left_join(
        codes_manual_nested,
        by = dplyr::join_by("group_id", "login_code", "login_name", "booklet_id", "unit_key")
      )

  } else {
    responses_inserted <- responses
  }

  responses_for_coding <-
    responses_inserted %>%
    tidyr::nest(unit_responses = -any_of(c("unit_key"))) %>%
    # Filter off units without coding scheme
    dplyr::semi_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::left_join(coding_schemes, by = dplyr::join_by("unit_key")) %>%
    dplyr::arrange(unit_key)

  final_unit_keys <- responses_for_coding$unit_key

  cli::cli_h3("Start coding")

  progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$final_unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

  progress_bar_format_done <- "Coded {cli::pb_total} unit{?s} in {cli::pb_elapsed}."

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
            final_unit_keys = final_unit_keys,
            n_units = n_units
          ),
          format = progress_bar_format,
          format_done = progress_bar_format_done,
          clear = FALSE
        ))
    )

  if (prepare) {
    tryCatch(
      error = function(cnd) {
        cli::cli_alert_danger("Preparation could not be completed. Returning coded data frame.",
                              wrap = TRUE)

        # Default return
        return(responses_coded)
      },
      # TODO: Does not work if no code_ids are available (e.g., only coding errors)
      responses_coded %>%
        dplyr::select(-dplyr::any_of(c("unit_responses", "coding_scheme"))) %>%
        tidyr::unnest(unit_codes) %>%
        dplyr::rename(variable_id = id, code_id = code) %>%
        tidyr::unnest(value, keep_empty = TRUE) %>%
        dplyr::left_join(pcs_variables, by = dplyr::join_by("unit_key", "variable_id")) %>%
        dplyr::left_join(pcs_codes %>% dplyr::select(unit_key, variable_id, code_id, code_type),
                         by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
        dplyr::rename(any_of(c(
          "code_score" = "score",
          "code_status" = "status"
        ))),
      finally = cli::cli_text("Time finished: {Sys.time()}")
    )
  } else {
    finish_time <- Sys.time()
    cli::cli_text("Time finished: {Sys.time()}")

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
  # Müssten einige Variablen nicht noch hoch?
  eatAutoCode::code_responses_array(coding_scheme, unit_responses) %>%
    dplyr::mutate(
      # Values need to be concatenated if they are still lists
      # Identifier is [] and elements are concatenated with a ,
      value = purrr::map(value, function(x) {
        if (length(x) > 1) {
          list_vals <- x %>% stringr::str_c(collapse = ",")
          as.character(stringr::str_glue("[[{list_vals}]]"))
        } else {
          as.character(x)
        }
      })) %>%
    tidyr::unnest(value)
}
