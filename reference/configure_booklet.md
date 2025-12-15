# Helper function to prepare booklet configuration header

Helper function to prepare booklet configuration header

## Usage

``` r
configure_booklet(
  loading_mode = c("lazy", "eager"),
  log_policy = c("rich", "disabled", "lean", "debug"),
  paging_mode = c("buttons", "separate", "concat-scroll", "concat-scroll-snap"),
  page_navibuttons = c("off", "separate_bottom"),
  unit_navibuttons = c("full", "arrows_only", "off"),
  unit_menu = c("off", "full"),
  force_presentation_complete = c("always", "on", "off"),
  force_responses_complete = c("OFF", "always", "on"),
  controller_design = c("2018", "2022"),
  unit_screenheader = c("empty", "with_unit_title", "with_block_title",
    "with_booklet_title", "off"),
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
)
```

## Arguments

- loading_mode:

  Tbd.

- log_policy:

  Tbd.

- paging_mode:

  Tbd.

- page_navibuttons:

  Tbd.

- unit_navibuttons:

  Tbd.

- unit_menu:

  Tbd.

- force_presentation_complete:

  Tbd.

- force_responses_complete:

  Tbd.

- controller_design:

  Tbd.

- unit_screenheader:

  Tbd.

- unit_title:

  Tbd.

- unit_show_time_left:

  Tbd.

- unit_time_left_warnings:

  Tbd.

- show_end_button_in_player:

  Tbd.

- restore_current_page_on_return:

  Tbd.

- allow_player_to_terminate_test:

  Tbd.

- lock_test_on_termination:

  Tbd.

- ask_for_fullscreen:

  Tbd.

- show_fullscreen_button:

  Tbd.

- show_reload_button:

  Tbd.

## Value

A list with a valid booklet configuration.
