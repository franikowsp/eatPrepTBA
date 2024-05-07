bookletConfig <- function(...) {
  ..configuration <-
    list(
      loading_mode = "LAZY",
      logPolicy = "rich",
      pagingMode = "buttons",
      page_navibuttons = "OFF",
      unit_navibuttons = "FULL",
      unit_menu = "OFF",
      force_presentation_complete = "ALWAYS",
      force_responses_complete = "OFF",
      controller_design = "2018",
      unit_screenheader = "EMPTY",
      unit_title = "ON",
      unit_show_time_left = "OFF",
      unit_time_left_warnings = "5",
      show_end_button_in_player = "OFF",
      restore_current_page_on_return = "OFF",
      allow_player_to_terminate_test = "ON",
      lock_test_on_termination = "OFF",
      ask_for_fullscreen = "ON",
      show_fullscreen_button = "ON"
    )

  ..add <- (...)
  if (any(names(..add) %in% names(..configuration))) {
    ..general_names <- intersect(names(..add), names(..configuration))

    ..configuration[..general_names] <- ..add[..general_names]
  }

  ..configuration %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("Config")
}
