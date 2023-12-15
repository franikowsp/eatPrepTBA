#' Prepares a booklet xml file
#'
#' @param template Path to the XML template of the booklet that will be filled with units.
#' @param id Booklet ID.
#' @param label Booklet label.
#' @param units Unit table to be assigned to the booklet.

#' @description
#' This function fills a booklet with units.
#'
#' @return NULL
#' @export
#'
#' @examples
#' @aliases
#' prepareBooklet
prepareBooklet <- function(template, id, label, units) {
  booklet_xml <- xml2::read_xml(template)

  # Add booklet metadata
  Metadata <-
    booklet_xml %>%
    xml2::xml_child("Metadata")

  Metadata %>%
    xml2::xml_add_child("Id") %>%
    xml2::xml_set_text(as.character(id))

  Metadata %>%
    xml2::xml_add_child("Label") %>%
    xml2::xml_set_text(as.character(label))

  # Add unit information
  Units <-
    booklet_xml %>%
    xml2::xml_child("Units")

  units %>%
    purrr::transpose() %>%
    purrr::walk(
      function(x) {
        Units %>%
          xml2::xml_add_child("Unit",
                              id = x$id,
                              label = x$label,
                              labelshort = x$labelshort)
      }
    )

  return(booklet_xml)
}
