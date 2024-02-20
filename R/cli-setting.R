# Vector print modification (other cli class definitions go here)
cli_setting <- function(.envir = parent.frame()) {
  cli::start_app(
    .auto_close = TRUE,
    .envir = .envir,
    theme = list(
      "body" = list("vec-trunc" = Inf),
      ".wsg-id" = list("color" = "#bef264"),
      ".wsg" = list("color" = "#84cc16"),
      ".ws-id" = list("color" = "#89b3ff"),
      ".ws" = list("color" = "#5793FF"),
      ".ws-id" = list("color" = "#89b3ff"),
      ".booklet" = list("color" = "#8BE836"),
      ".unit" = list("color" = "#E83938"),
      ".testtaker" = list("color" = "#FFC142")
    )
  )
}
