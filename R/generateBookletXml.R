#' Generates booklets XML from unit information
#'
#' @param Id
#' @param Label
#' @param Description
#' @param BookletConfig
#' @param units
#' @param testlets
#'
#' @return
#' @export
#'
#' @examples
generateBookletXml <- function(Id, Label, Description = NULL, BookletConfig = NULL,
                               units = NULL,
                               testlets = NULL) {
  cli_setting()

  if (is.null(BookletConfig)) {
    BookletConfig <- bookletConfig()
  } else {
    BookletConfig <- bookletConfig(BookletConfig)
  }

  # Add nodes
  Metadata <-
    list(
      Id = list(Id),
      Label = list(Label)
    )

  if (!is.null(Description)) {
    Metadata$Description <- list(Description)
  }

  if ((is.null(units) & is.null(testlets)) |
      (! is.null(units) & ! is.null(testlets))) {
    cli::cli_alert_danger("Either {.arg units} or {.arg testlets} or must be specified.")
  } else {
    if (! is.null(units)) {
      Units <-
        units %>% prepareUnits()
    } else if (! is.null(testlets)) {
      Units <-
        testlets %>% prepareTestlets()
    }
  }

  # Get nodes together
  list(
    Booklet = list(
      list(Metadata = Metadata,
           BookletConfig = BookletConfig,
           Units = Units),
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
      "xsi:noNamespaceSchemaLocation" = "https://raw.githubusercontent.com/iqb-berlin/testcenter/15.0.0/definitions/vo_Booklet.xsd"
    ))  %>%
    list2Xml() %>%
    xml2::as_xml_document()
}

prepareRestrictions <- function(code_to_enter = NULL,
                                code = NULL,
                                minutes = NULL,
                                presentation = NULL,
                                response = NULL) {
  # Following XML Restrictions structure
  Restrictions <-
    list(
      CodeToEnter = list(
        list(code_to_enter),
        code = code
      ),
      TimeMax = list(
        minutes = minutes
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

prepareUnits <- function(units) {
  units %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names("Unit")
}

prepareTestlets <- function(testlets) {
  prepared_testlets <-
    testlets %>%
    dplyr::select(id, label) %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE)

  prepared_units <-
    testlets %>%
    dplyr::pull(units) %>%
    purrr::map(prepareUnits)

  prepared_restrictions <-
    testlets %>%
    dplyr::pull(restrictions) %>%
    purrr::map(function(x) {
      restrictions_list <-
        x %>%
        as.list()

      do.call(prepareRestrictions, args = restrictions_list)
    })

  node_names <- c()
  for (i in seq_along(prepared_testlets)) {
    current_units <- prepared_units[[i]]
    current_restrictions <- prepared_restrictions[[i]]

    if (is.na(prepared_testlets[[i]]$id)) {
      prepared_testlets[[i]] <- current_units$Unit

      node_names[[i]] <- "Unit"
    } else {
      prepared_testlets[[i]] <- c(prepared_testlets[[i]],
                                  list(current_restrictions), list(current_units))

      node_names[[i]] <- "Testlet"
    }
  }

  prepared_testlets %>%
    purrr::set_names(node_names)
}
