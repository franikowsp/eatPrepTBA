#' Helper function to prepare booklet configuration header
#'
#' @param loading_mode Tbd.
#' @param log_policy Tbd.
#' @param paging_mode Tbd.
#' @param page_navibuttons Tbd.
#' @param unit_navibuttons Tbd.
#' @param unit_menu Tbd.
#' @param force_presentation_complete Tbd.
#' @param force_responses_complete Tbd.
#' @param controller_design Tbd.
#' @param unit_screenheader Tbd.
#' @param unit_title Tbd.
#' @param unit_show_time_left Tbd.
#' @param unit_time_left_warnings Tbd.
#' @param show_end_button_in_player Tbd.
#' @param restore_current_page_on_return Tbd.
#' @param allow_player_to_terminate_test Tbd.
#' @param lock_test_on_termination Tbd.
#' @param ask_for_fullscreen Tbd.
#' @param show_fullscreen_button Tbd.
#' @param show_reload_button Tbd.
#'
#' @return A list with a valid booklet configuration.
#'
#' @keywords internal
configure_booklet <- function(
    loading_mode = c("lazy", "eager"),
    log_policy = c("rich", "disabled", "lean", "debug"),
    paging_mode = c("buttons", "separate", "concat-scroll", "concat-scroll-snap"),
    page_navibuttons = c("off", "separate_bottom"),
    unit_navibuttons = c("full", "arrows_only", "off"),
    unit_menu = c("off", "full"),
    force_presentation_complete = c("always", "on", "off"),
    force_responses_complete = c("OFF", "always", "on"),
    controller_design = c("2018", "2022"),
    unit_screenheader = c("empty",
                          "with_unit_title",
                          "with_block_title",
                          "with_booklet_title",
                          "off"),
    unit_title = c("on", "off"),
    unit_show_time_left = c("off", "on"),
    unit_time_left_warnings = c(5),
    show_end_button_in_player = c("off", "always", "on_last_unit"),
    restore_current_page_on_return = c("off", "on"),
    allow_player_to_terminate_test = c("on", "last_unit", "off"),
    lock_test_on_termination = c("off", "on"),
    ask_for_fullscreen = c("on", "off"),
    show_fullscreen_button = c("on", "off"),
    show_reload_button = c("on", "off")
) {
  ..configuration <-
    list(
      loading_mode = stringr::str_to_upper(match.arg(loading_mode)),
      logPolicy = match.arg(log_policy),
      pagingMode = match.arg(paging_mode),
      page_navibuttons = stringr::str_to_upper(match.arg(page_navibuttons)),
      unit_navibuttons = stringr::str_to_upper(match.arg(unit_navibuttons)),
      unit_menu = stringr::str_to_upper(match.arg(unit_menu)),
      force_presentation_complete = stringr::str_to_upper(match.arg(force_presentation_complete)),
      force_responses_complete = stringr::str_to_upper(match.arg(force_responses_complete)),
      controller_design = stringr::str_to_upper(match.arg(controller_design)),
      unit_screenheader = stringr::str_to_upper(match.arg(unit_screenheader)),
      unit_title = stringr::str_to_upper(match.arg(unit_title)),
      unit_show_time_left = stringr::str_to_upper(match.arg(unit_show_time_left)),
      unit_time_left_warnings = unit_time_left_warnings,
      show_end_button_in_player = stringr::str_to_upper(match.arg(show_end_button_in_player)),
      restore_current_page_on_return = stringr::str_to_upper(match.arg(restore_current_page_on_return)),
      allow_player_to_terminate_test = stringr::str_to_upper(match.arg(allow_player_to_terminate_test)),
      lock_test_on_termination = stringr::str_to_upper(match.arg(lock_test_on_termination)),
      ask_for_fullscreen = stringr::str_to_upper(match.arg(ask_for_fullscreen)),
      show_fullscreen_button = stringr::str_to_upper(match.arg(show_fullscreen_button)),
      show_reload_button = stringr::str_to_upper(match.arg(show_reload_button))
    )

  ..configuration %>%
    purrr::imap(function(x, n) {
      list(
        list(x),
        key = n
      )
    }) %>%
    purrr::set_names("Config")
}
