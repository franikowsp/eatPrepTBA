#' Generates testtakers XML from unit information
#'
#' @param testtakers Must be a data frame with the columns ...
#' @param custom_texts Optional. List of custom texts to be modified.
#' @param app_version Version of the target Testcenter instance.
#'
#' @return A testtakers XML.
#'
#' @export
generate_testtakers <- function(testtakers,
                                custom_texts = NULL,
                                app_version = "15.6.0") {
  cli_setting()

  if (!is.null(custom_texts) & length(custom_texts) > 0) {
    CustomTexts <- rlang::exec("customize_texts", !!!custom_texts)
  } else {
    CustomTexts <- list()
  }

  # Add nodes
  Metadata <- list()

  TesttakerGroups <- prepare_testtaker_groups(testtakers)

  # Get nodes together
  Testtakers <-
    list(
      Testtakers = list(
        c(list(Metadata = Metadata,
               CustomTexts = CustomTexts),
          TesttakerGroups),
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xsi:noNamespaceSchemaLocation" = stringr::str_glue("https://raw.githubusercontent.com/iqb-berlin/testcenter/{app_version}/definitions/vo_Testtakers.xsd")
      ))

  # Bug in XSD scheme (CustomTexts MUST be filled)
  if (length(Testtakers$Testtakers[[1]]$CustomTexts) == 0) {
    Testtakers$Testtakers[[1]]$CustomTexts <- NULL
  }

  Testtakers %>%
    list_to_xml() %>%
    xml2::as_xml_document()
}

# needs to be rebuilt to allow for booklets
prepare_testtaker_groups <- function(testtakers) {
  # Group nodes
  group_variables <- c("label", "validTo", "validFrom", "validFor")

  groups <-
    testtakers %>%
    dplyr::select(
      id,
      dplyr::any_of(group_variables)
    ) %>%
    dplyr::distinct()

  # Login nodes
  login_variables <- c("name", "pw", "mode")

  logins <-
    testtakers %>%
    dplyr::select(
      id,
      dplyr::any_of(login_variables)
    ) %>%
    dplyr::distinct()

  # Booklet nodes
  booklet_variables <- c("booklet", "codes")

  booklets <-
    testtakers %>%
    dplyr::select(
      name,
      dplyr::any_of(booklet_variables)
    ) %>%
    dplyr::distinct()

  # Preparation of nodes
  groups_prep <-
    groups %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "id"))

  logins_prep <-
    logins %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "name")) %>%
    # Deletes empty nodes (including passwords set to NA)
    purrr::modify_tree(leaf = function(x) if(is.na(x)) NULL else x,
                       post = purrr::compact)

  booklets_prep <-
    booklets %>%
    as.list() %>%
    purrr::list_transpose(simplify = FALSE) %>%
    purrr::set_names(purrr::map(., "name")) %>%
    purrr::map(function(x) purrr::list_modify(x, name = purrr::zap())) %>%
    # TODO: This line avoids that codes can be added here
    purrr::map(purrr::list_transpose) %>%
    purrr::map_depth(2, as.list) %>%
    purrr::map_depth(2, function(x) {
      is_booklet_name <- names(x) == "booklet"
      booklet <- x[is_booklet_name] %>%
        purrr::set_names(NULL)

      c(list(booklet), x[!is_booklet_name])
    })

  # Insertion of child nodes in parent nodes
  names_login <- names(logins_prep)
  names_booklet <- names(booklets_prep)

  logins_insert <-
    logins_prep %>%
    purrr::imap(function(x, i) {
      BookletMerge <-
        booklets_prep[names_booklet == i] %>%
        purrr::map(purrr::set_names, "Booklet") %>%
        # purrr::pluck(i) %>%
        # Changed from == to %in%
        purrr::keep(names(.) %in% names_booklet) %>%
        unname(.) %>%
        purrr::reduce(c) %>%
        list(.)

      c(x, BookletMerge)
    }) %>%
    purrr::set_names(purrr::map(., "id")) %>%
    purrr::map(function(x) purrr::list_modify(x, id = purrr::zap()))

  ids_login <- names(logins_insert)

  groups_prep %>%
    purrr::imap(function(x, i) {
      LoginMerge <- logins_insert[ids_login == i] %>%
        purrr::set_names("Login") %>%
        list(.)

      c(x, LoginMerge)
    }) %>%
    purrr::set_names("Group")
}
