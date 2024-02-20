bookletConfig <- function(...) {
  list(
    list(
      list("LAZY"),
      key = "loading_mode"
    ),
    list(
      list("rich"),
      key = "logPolicy"
    ),
    list(
      list("buttons"),
      key = "pagingMode"
    ),
    list(
      list("OFF"),
      key = "page_navibuttons"
    ),
    list(
      list("FULL"),
      key = "unit_navibuttons"
    ),
    list(
      list("OFF"),
      key = "unit_menu"
    ),
    list(
      list("ALWAYS"),
      key = "force_presentation_complete"
    ),
    list(
      list("OFF"),
      key = "force_responses_complete"
    ),
    list(
      list("2018"),
      key = "controller_design"
    ),
    list(
      list("EMPTY"),
      key = "unit_screenheader"
    ),
    list(
      list("ON"),
      key = "unit_title"
    ),
    list(
      list("OFF"),
      key = "unit_show_time_left"
    ),
    list(
      list("5"),
      key = "unit_time_left_warnings"
    ),
    list(
      list("OFF"),
      key = "show_end_button_in_player"
    ),
    list(
      list("OFF"),
      key = "restore_current_page_on_return"
    ),
    list(
      list("ON"),
      key = "allow_player_to_terminate_test"
    ),
    list(
      list("OFF"),
      key = "lock_test_on_termination"
    ),
    list(
      list("ON"),
      key = "ask_for_fullscreen"
    ),
    list(
      list("ON"),
      key = "show_fullscreen_button"
    ),
    ...
  ) %>%
    purrr::set_names("Config")
}
