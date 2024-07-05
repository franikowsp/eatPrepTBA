# Vector print modification (other cli class definitions go here)
cli_setting <- function(.envir = parent.frame()) {
  cli::start_app(
    .auto_close = TRUE,
    .envir = .envir,
    theme = list(
      "body" = list("vec-trunc" = Inf),
      ".user-label" = list("color" = "#a855f7"),
      ".user-name" = list("color" = "#d8b4fe"),
      ".wsg-id" = list("color" = "#bef264"),
      ".wsg-label" = list("color" = "#84cc16"),
      ".ws-id" = list("color" = "#89b3ff"),
      ".ws-label" = list("color" = "#5793FF"),
      ".ws-id" = list("color" = "#89b3ff"),
      ".booklet" = list("color" = "#8BE836"),
      ".unit" = list("color" = "#E83938"),
      ".testtaker" = list("color" = "#FFC142")
    )
  )
}
