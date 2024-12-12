# TODO: This is only necessary due to bugs in older TC versions (2024)

# The most preferred state comes first
laststates_lookup <-
  list(
    RESPONSE_PROGRESS = c(
      "complete",
      "some",
      "none"
    ),
    PRESENTATION_PROGRESS = c(
      "complete",
      "some",
      "none"
    ),
    PLAYER = c(
      "RUNNING",
      "LOADING"
    )
  )


update_laststate <- function(information, current_states) {
  # Always return the highest available state
  intersect(laststates_lookup[[information]], current_states)[1]
}
