#' Generates booklets XML from unit information
#'
#' @param Id
#' @param Label
#' @param BookletConfig
#' @param units
#'
#' @return
#' @export
#'
#' @examples
generateBookletXml <- function(Id, Label, BookletConfig = NULL, units) {
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

  Units <-
    units %>% prepareUnits()

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

# needs to be rebuilt to allow for booklets
prepareUnits <- function(units) {
  units %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names("Unit")
}
