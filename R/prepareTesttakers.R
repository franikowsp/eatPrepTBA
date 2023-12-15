#' Prepares a testtakers xml file
#'
#' @param template Path to the XML template of the testtakers file that will be filled with login information.
#' @param testtakers Testtakers table to be assigned to the testtakers file.

#' @description
#' This function fills a testtakers file with login information.
#'
#' @return NULL
#' @export
#'
#' @examples
#' @aliases
#' prepareTesttakers
prepareTesttakers <- function(template, testtakers) {
  testtakers_xml <- xml2::read_xml(template)

  testtakers_list <-
    testtakers %>%
    tidyr::nest(data = -c(id, label)) %>%
    dplyr::mutate(
      data = purrr::map(data, purrr::transpose)
    ) %>%
    purrr::transpose()

  # Add group information
  Groups <- vector(mode = "list", length = length(testtakers_list))

  for (i in seq_along(Groups)) {
    group <- testtakers_list[[i]]

    Groups[[i]] <- xml2::xml_new_root("Group", id = group$id, label = group$label)

    Logins <- vector(mode = "list", length = length(group$data))

    for (j in seq_along(Logins)) {
      testtaker <- group$data[[j]]

      Login <-
        Groups[[i]] %>%
        xml2::xml_add_child("Login",
                            mode = testtaker$mode,
                            name = testtaker$name,
                            pw = testtaker$pw)

      Booklets <- vector(mode = "list", length = length(testtaker$booklets))

      for (k in seq_along(Booklets)) {
        booklet <- testtaker$booklets[[k]]

        Login %>%
          xml2::xml_add_child("Booklet", codes = booklet$code) %>%
          xml2::xml_set_text(booklet$booklet)
      }
    }

    testtakers_xml %>%
      xml2::xml_add_child(.value = Groups[[i]])
  }


  return(testtakers_xml)
}
