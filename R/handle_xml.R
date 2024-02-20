# Prepares lists as XML
handle_xml <- function(x, name, ...) {
  if (!is.null(x$text)) {
    text <- list(x$text)
    x$text <- NULL
  } else {
    text <- list()
  }

  # Eliminates list elements (potential child nodes)
  x_no_list <-
    x %>%
    purrr::keep(function(x) typeof(x) != "list")

  x_children <-
    x %>%
    purrr::keep(function(x) typeof(x) == "list")

  .data <- c(text)

  output <-
    list(do.call(structure, args = c(.Data = list(.data), x_no_list))) %>%
    # purrr::pluck(1) %>%
    purrr::set_names(name)

  output
}

# Prepares lists as XML
handle_xml_map <- function(x, name = NULL) {
  # if (is.null(name)) {
  #   node_name <- x$node
  # } else {
  #   node_name <- name
  # }
  # x <- within(x, rm("node"))

  x %>%
    purrr::map(handle_xml, name = name)
}
