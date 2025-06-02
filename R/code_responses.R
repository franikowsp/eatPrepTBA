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

  # cli::cli_h3("Prepare responses")

  units_prep <-
    units %>%
    dplyr::filter(
      unit_key %in% responses$unit_key
    ) %>%
    dplyr::select(ws_id, ws_label, unit_key, unit_id, unit_label, coding_scheme, unit_variables)

  no_coding_schemes <-
    units_prep %>%
    dplyr::filter(is.na(coding_scheme))

  coding_schemes <-
    units_prep %>%
    dplyr::filter(!is.na(coding_scheme))

  unit_keys_no_cs <- no_coding_schemes$unit_key
  n_units_no_cs <- length(unique(unit_keys_no_cs))

  if (n_units_no_cs > 0) {
    cli::cli_alert_info("Identified {n_units_no_cs} units with no coding scheme:")
    cli::cli_alert("{.unit-key {unit_keys_no_cs}}")
  }

  unit_keys <- coding_schemes$unit_key
  n_units <- length(unique(unit_keys))
  cli::cli_alert_success("Identified {n_units} units that can be coded.")

  # Insert manual codes
  if (!is.null(codes_manual) || prepare) {
    cli::cli_h3("Prepare coding schemes")

    coding_schemes_prepared <-
      coding_schemes %>%
      add_coding_scheme() %>%
      dplyr::select(unit_key, variable_id, variable_source_type, variable_codes)

    pcs_variables <-
      coding_schemes_prepared %>%
      dplyr::distinct(unit_key, variable_id, variable_source_type)

    pcs_codes <-
      coding_schemes_prepared %>%
      tidyr::unnest(variable_codes) %>%
      dplyr::distinct(unit_key, variable_id,
                      code_id, code_type, code_score) #%>%
    # dplyr::filter(!is.na(code_id))
  }

  if (!is.null(codes_manual)) {
    cli::cli_h3("Prepare manual codes")

    # pcs_variables_insert <-
    #   pcs_variables %>%
    #   dplyr::select(-source_type)

    pcs_codes_insert <-
      pcs_codes %>%
      dplyr::mutate(status = "CODING_COMPLETE") %>%
      dplyr::select(-code_type)

    codes_manual_prepared <-
      codes_manual %>%
      dplyr::select(any_of(c(
        "group_id",
        "booklet_id",
        "login_code",
        "login_name",
        "variable_id",
        "unit_key",
        "code_id"
      ))) %>%
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

    codes_manual_nested <-
      codes_manual_prepared %>%
      dplyr::rename(id = variable_id, code = code_id, score = code_score) %>%
      tidyr::nest(
        manual = c(id, code, score, status)
      ) %>%
      dplyr::arrange(unit_key)

    manual_unit_keys <- codes_manual_nested$unit_key

    codes_manual_json <-
      codes_manual_nested %>%
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
      )

    # TODO: Add filter for unit_keys that are only in coding_schemes, also needs to be arranged
    responses_inserted <-
      responses %>%
      dplyr::left_join(
        codes_manual_json,
        by = dplyr::join_by("group_id", "login_code", "login_name", "booklet_id", "unit_key")
      )

  } else {
    responses_inserted <- responses
  }

  coding_schemes_merge <-
    coding_schemes %>%
    dplyr::select(unit_key, coding_scheme)

  responses_for_coding <-
    responses_inserted %>%
    tidyr::nest(unit_responses = -any_of(c("unit_key"))) %>%
    # Filter off units without coding scheme
    dplyr::semi_join(coding_schemes_merge, by = dplyr::join_by("unit_key")) %>%
    dplyr::left_join(coding_schemes_merge, by = dplyr::join_by("unit_key")) %>%
    dplyr::arrange(unit_key)

  final_unit_keys <- responses_for_coding$unit_key

  cli::cli_h3("Start coding")

  progress_bar_format <- "Coding unit {.unit-key {cli::pb_extra$final_unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"

  progress_bar_format_done <- "Coded {cli::pb_total} unit{?s} in {cli::pb_elapsed}."

  # unit_responses <- responses_for_coding %>% dplyr::slice(123) %>% purrr::pluck("unit_responses", 1)
  # coding_scheme <- responses_for_coding %>% dplyr::slice(123) %>% purrr::pluck("coding_scheme")

  # save(unit_responses, coding_scheme, file = "D:/data/test-vera.RData")

  # unit_responses <- test$unit_responses[[1]]
  # coding_scheme <- test$coding_scheme[[1]]

  responses_coded <-
    responses_for_coding %>% #dplyr::slice(2) ->test
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
    ) %>%
    dplyr::select(-dplyr::any_of(c("unit_responses", "coding_scheme"))) %>%
    tidyr::unnest(unit_codes) %>%
    dplyr::rename(any_of(c(
      "variable_id" = "id",
      "code_id" = "code",
      "code_score" = "score",
      "code_status" = "status"
    )))

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
        tidyr::unnest(value, keep_empty = TRUE) %>%
        dplyr::left_join(pcs_variables, by = dplyr::join_by("unit_key", "variable_id")) %>%
        dplyr::left_join(pcs_codes %>% dplyr::select(unit_key, variable_id, code_id, code_type),
                         by = dplyr::join_by("unit_key", "variable_id", "code_id")) %>%
        dplyr::relocate(
          dplyr::any_of(c("variable_source_type")),
          .after = c("variable_id")
        ) %>%
        dplyr::relocate(
          dplyr::any_of(c("code_id",
                          "code_score",
                          "code_status",
                          "code_type")),
          .after = c("value")
        ),
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
          stringr::str_glue("[[{list_vals}]]")
        } else {
          x
        }
      })) %>%
    tidyr::unnest(value, keep_empty = TRUE)
}
