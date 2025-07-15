#' Estimates stay times from log data
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#'
#' @return Contains the estimated stay times of units and pages.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns estimated stay times for units and unit pages.
#'
#' @export
estimate_unit_times <- function(logs) {
  cli_setting()

  groups_booklet <- setdiff(names(logs), c("unit_key", "unit_alias", "ts", "log_entry"))
  groups_unit <- setdiff(names(logs), c("ts", "log_entry"))

  all_logs <-
    logs %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)"),
      # This is only a constant message stream that is not interaction-based
      !log_entry %>% stringr::str_detect("TESTLETS_TIMELEFT")
    ) %>%
    dplyr::mutate(ts = as.numeric(ts)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups_booklet, "ts")))) %>%
    # Unusable timestamps
    dplyr::filter(ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      unit_key = dplyr::case_when(
        # For legacy reasons
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~
          stringr::str_extract(log_entry, "\"(.+)\"", group = TRUE),
        .default = unit_key
      ),
    ) %>%
    dplyr::filter(!is.na(unit_key) & unit_key != "") %>%
    dplyr::ungroup()

  all_ts <-
    all_logs %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
        # For the previous unit
        stringr::str_detect(log_entry, "CURRENT_UNIT_ID") ~ "unit_current_ts",
        stringr::str_detect(log_entry, "PLAYER = LOADING") ~ "unit_load_ts",
        stringr::str_detect(log_entry, "PLAYER = RUNNING") ~ "unit_start_ts",
        stringr::str_detect(log_entry, "CURRENT_PAGE_ID") ~ "page_start_ts",
        log_entry == "PLAYER = PAUSED" ~ "n_paused",
        log_entry == "FOCUS : \"HAS_NOT\"" ~ "n_lost_focus",
        .default = NA_character_
      ),
      page_id = dplyr::case_when(
        # For legacy reasons
        log_entry == "CURRENT_PAGE_ID" ~ 0L,
        ts_name == "page_start_ts" ~ log_entry %>% stringr::str_extract("\\d+") %>% as.integer(),
        .default = NA_integer_
      )
    ) %>%
    dplyr::group_by(dplyr::across(c(groups_unit, "ts_name"))) %>%
    dplyr::mutate(
      n_start = ifelse(ts_name == "unit_start_ts", seq_along(ts_name), NA_integer_)
    ) %>%
    dplyr::group_by(dplyr::across(groups_unit)) %>%
    tidyr::fill(n_start) %>%
    dplyr::group_by(dplyr::across(c(groups_unit, "n_start"))) %>%
    dplyr::mutate(
      n_ts = seq_along(ts_name),
      ts_name = ifelse(n_ts == max(n_ts), "unit_end_ts", ts_name),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ts_name))

  unit_logs <-
    all_ts %>%
    dplyr::filter(
      ts_name == "unit_start_ts" | ts_name == "unit_current_ts"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      ts_prev = dplyr::lag(ts),
      ts_next = dplyr::lead(ts),
      unit_time = ts_next - ts
    ) %>%
    dplyr::filter(ts_name %in% c("unit_start_ts")) %>%
    dplyr::group_by(
      dplyr::across(groups_unit)
    ) %>%
    dplyr::summarise(
      unit_start_time = min(ts),
      unit_time = sum(unit_time, na.rm = TRUE),
      unit_n_start = length(unit_n_start)
    ) %>%
    dplyr::ungroup()

  if (any(!is.na(all_ts$page_id))) {
    unit_page_logs <-
      all_ts %>%
      dplyr::filter(
        ts_name %>% stringr::str_detect("^page_") | ts_name == "unit_start_ts" | ts_name == "unit_current_ts"
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groups_booklet)))) %>%
      dplyr::mutate(
        ts_prev = dplyr::lag(ts),
        ts_next = dplyr::lead(ts),
        page_time = ts_next - ts
        # ts = ifelse(ts_name == "page_start_ts", ts, ts_next)
      ) %>%
      dplyr::group_by(dplyr::across(groups_unit)) %>%
      dplyr::filter(
        ts_name != "unit_current_ts"
      ) %>%
      # The first page is not logged before completion...
      tidyr::fill(page_id, .direction = "up") %>%
      # Incomplete loads
      dplyr::filter(!is.na(page_id)) %>%
      dplyr::group_by(dplyr::across(c(groups_unit, "page_id"))) %>%
      dplyr::summarise(
        page_start_time = min(page_time),
        page_time = sum(page_time),
        page_n_start = length(page_time),
      ) %>%
      dplyr::ungroup() %>%
      tidyr::nest(unit_page_logs = dplyr::any_of(c("page_id", "page_time", "page_n_start")))

    unit_logs %>%
      dplyr::left_join(
        unit_page_logs,
        by = dplyr::join_by(!!! groups_unit)
      ) %>%
      dplyr::mutate(
        unit_has_pages = purrr::map_lgl(unit_page_logs, function(x) !is.null(x))
      )
  } else {
    unit_logs
  }


}

