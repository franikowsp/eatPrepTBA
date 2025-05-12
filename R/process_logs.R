#' Estimates stay times from log data
#'
#' @param logs Tibble. Must be a logs tibble retrieved with `get_logs()` or `read_logs()`.
#'
#' @return Contains the estimated stay times of units.
#'
#' @export
process_logs <- function(logs) {
  cli_setting()

  groups_booklet <- setdiff(names(logs), c("unit_key", "unit_alias", "ts", "log_entry"))
  groups_unit <- setdiff(names(logs), c("ts", "log_entry"))

  # TODO: Beispiel verallgemeinern
  # codes <- unique(logs$login_code)[1]

  # TODO: What about incomplete loads??
  # logs %>% dplyr::filter(login_code == "5zmd", booklet_id == "THD048_1_T2") %>% View


  # logs %>% dplyr::filter(booklet_id %>% stringr::str_detect("^THD")) %>% View
  all_logs <-
    logs %>%
    # TODO: Remove
    dplyr::filter(booklet_id %>% stringr::str_detect("^THD")) %>%
    ## REMOVE
    dplyr::mutate(ts = as.numeric(ts)) %>%
    dplyr::arrange(dplyr::across(c(groups_unit, "ts"))) %>%
    # Unusable timestamps
    dplyr::filter(ts != 0) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      ts_prev = dplyr::lag(ts),
      ts_next = dplyr::lead(ts),
    ) %>%
    dplyr::filter(!is.na(unit_key) & unit_key != "") %>%
    dplyr::ungroup()

  all_ts <-
    all_logs %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(
          c(groups_unit, "log_entry")
        )
      )
    ) %>%
    dplyr::filter(
      # Delete duplicate page identifiers as these would contaminate page time estimation
      !log_entry %>% stringr::str_detect("(CURRENT_PAGE_NR|PAGE_COUNT)")
    ) %>%
    dplyr::mutate(
      ts_name = dplyr::case_when(
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
        ts_name == "page_start_ts" ~ log_entry %>% stringr::str_extract("\\d") %>% as.integer(),
        .default = NA_integer_
      )
    ) %>%
    dplyr::group_by(dplyr::across(c(groups_unit, "ts_name"))) %>%
    dplyr::mutate(
      n_load = ifelse(ts_name == "unit_load_ts", seq_along(ts_name), NA_integer_)
    ) %>%
    dplyr::group_by(dplyr::across(groups_unit)) %>%
    tidyr::fill(n_load) %>%
    dplyr::group_by(dplyr::across(c(groups_unit, "n_load"))) %>%
    dplyr::mutate(
      n_ts = seq_along(ts_name),
      ts_name = ifelse(n_ts == max(n_ts), "unit_end_ts", ts_name),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(ts_name))

  unit_logs <-
    all_ts %>%
    dplyr::filter(ts_name %>% stringr::str_detect("^unit_")) %>%
    dplyr::mutate(
      ts = ifelse(ts_name == "unit_start_ts", ts, ts_next)
    ) %>%
    dplyr::select(c(groups_unit, "ts_name", "ts", "n_load")) %>%
    tidyr::pivot_wider(names_from = ts_name, values_from = ts, values_fn = as.numeric) %>%
    dplyr::mutate(
      time_unit = unit_end_ts - unit_start_ts,
      # time_unit = time_unit / 1000
    ) %>%
    dplyr::group_by(
      dplyr::across(groups_unit)
    ) %>%
    dplyr::summarise(
      time_unit = sum(time_unit),
      unit_end_ts = min(unit_load_ts),
      unit_start_ts = min(unit_start_ts),
      unit_end_ts = min(unit_end_ts),
      n_load = max(n_load)
    ) %>%
    dplyr::ungroup()

  # unit_page_logs %>% dplyr::filter(time_page < 0)

  unit_page_logs <-
    all_ts %>%
    dplyr::group_by(dplyr::across(groups_unit)) %>%
    dplyr::filter(any(!is.na(page_id))) %>%
    dplyr::filter(
      ts_name %>% stringr::str_detect("^page_") | ts_name == "unit_end_ts"
    ) %>%
    # Sometimes, the page message was the last signal
    tidyr::fill(page_id) %>%
    dplyr::ungroup() %>%
    # Incomplete loads
    dplyr::filter(!is.na(page_id)) %>%
    dplyr::mutate(
      ts = ifelse(ts_name == "page_start_ts", ts, ts_next)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(groups_booklet)))) %>%
    dplyr::mutate(
      page_end_ts = dplyr::lead(ts),
    ) %>%
    dplyr::filter(!is.na(ts_next)) %>%
    dplyr::mutate(
      dplyr::across(c("ts", "page_end_ts"), as.numeric),
      time_page = page_end_ts - ts
    ) %>%
    # dplyr::select(c(groups_unit, "ts_name", "ts", "page_id", "n_load")) %>%
    # dplyr::filter(ts_name %>% stringr::str_detect("^unit_")) %>%
    # tidyr::pivot_wider(names_from = ts_name, values_from = ts, values_fn = as.numeric) %>%
    # dplyr::mutate(
    #   time_unit = unit_end_ts - unit_start_ts,
    #   # time_unit = time_unit / 1000
    dplyr::group_by(dplyr::across(c(groups_unit, "page_id"))) %>%
    dplyr::summarise(
      time_page = sum(time_page),
      page_start_ts = min(ts),
      page_end_ts = min(page_end_ts),
      # n_load = max(n_load)
    ) %>%
    dplyr::ungroup()

  # Counts
  # count_events <-
  #   all_logs %>%
  #   dplyr::select(-log_entry) %>%
  #   dplyr::rename(
  #     log_entry = "ts_name"
  #   ) %>%
  #   dplyr::filter(
  #     stringr::str_detect(log_entry, "^n_")
  #   ) %>%
  #   dplyr::distinct() %>%
  #   dplyr::mutate(
  #     log_entry = factor(log_entry, levels = c(
  #       "n_paused",
  #       "n_lost_focus"
  #     ))
  #   ) %>%
  #   dplyr::group_by(
  #     dplyr::across(
  #       dplyr::any_of(
  #         c(groups, "log_entry")
  #       )
  #     )
  #   ) %>%
  #   dplyr::count(log_entry) %>%
  #   tidyr::pivot_wider(names_from = log_entry, values_from = n)
#
#   single_events %>% view
#
#   # Zeiten spreaden
#   single_events <-
#     all_logs %>%
#     dplyr::select(-log_entry) %>%
#     dplyr::rename(
#       log_entry = "ts_name"
#     ) %>%
#     dplyr::filter(stringr::str_detect(log_entry, "^ts_")) %>%
#     dplyr::distinct() %>%
#     dplyr::mutate(
#       log_entry = factor(log_entry, levels = c(
#         "ts_start",
#         "ts_end",
#         "ts_responses_complete",
#         "ts_responses_some",
#         "ts_player_running"
#       ))
#     ) %>%
#     dplyr::group_by(
#       dplyr::across(
#         dplyr::any_of(groups)
#       )
#     ) %>%
#     tidyr::pivot_wider(names_from = log_entry, values_from = ts) %>%
#     dplyr::mutate(
#       duration_stay = ts_end - ts_start,
#       duration_player_load = ts_player_running - ts_start,
#       duration_responses_some = ts_responses_some - ts_start,
#       duration_responses_complete = ts_responses_complete - ts_start
#     ) %>%
#     dplyr::ungroup()
#
#   single_events %>% print(width = Inf)
#
#   # Spreaden funktioniert hier nicht mehr
#   multiple_events <-
#     all_logs %>%
#     dplyr::filter(stringr::str_detect(ts_name, "^page_")) %>%
#     dplyr::distinct() %>%
#     dplyr::mutate(
#       page_id = stringr::str_remove(log_entry, "CURRENT_PAGE_ID( = )?"),
#       page_id = ifelse(page_id == "", 0, page_id) %>% as.integer()
#     ) %>%
#     dplyr::select(-c(log_entry, ts_name)) %>%
#     dplyr::group_by(
#       dplyr::across(
#         dplyr::any_of(groups)
#       )
#     ) %>%
#     # Achtung: Dieser Zeitschätzer ist nicht verlässlich!
#     dplyr::left_join(single_events %>% dplyr::select(groups, ts_end)) %>%
#     dplyr::mutate(
#       ts_page_start = ts,
#       ts_page_next = dplyr::lead(ts),
#       ts_page_next = ifelse(is.na(ts_page_next), ts_end, ts_page_next)
#     ) %>%
#     dplyr::select(-ts_end, -ts) %>%
#     dplyr::mutate(
#       duration_page = ts_page_next - ts_page_start
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-c(ts_page_next, ts_page_start)) %>%
#     tidyr::nest(page_logs = -groups)
#
#   multiple_events$page_logs[[1]]
#
#   single_events %>%
#     dplyr::left_join(multiple_events) %>%
#     dplyr::left_join(count_events)
}
