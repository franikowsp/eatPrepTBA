#' Helper function to prepare testlet restrictions
#'
#' @param code_to_enter Tbd.
#' @param code Tbd.
#' @param minutes Tbd.
#' @param leave Tbd.
#' @param presentation Tbd.
#' @param response Tbd.
#'
#' @return A list with a valid testlet restrictions.
#'
#' @keywords internal
restrict_testlet <- function(code_to_enter = NULL,
                             code = NULL,
                             minutes = NULL,
                             leave = NULL,
                             presentation = NULL,
                             response = NULL) {
  if (!is.null(code)) {
    if (is.null(code_to_enter)) {
      code_to_enter <- "Bitte Freigabewort eingeben!"
    }
  } else {
    code_to_enter <- NULL
  }

  # Following XML Restrictions structure
  Restrictions <-
    list(
      CodeToEnter = list(
        list(code_to_enter),
        code = code
      ),
      TimeMax = list(
        minutes = minutes,
        leave = leave
      ),
      DenyNavigationOnIncomplete = list(
        presentation = presentation,
        response = response
      )
    ) %>%
    purrr::modify_tree(post = purrr::compact)

  if (length(Restrictions) == 0) {
    NULL
  } else {
    list(Restrictions = Restrictions)
  }
}
