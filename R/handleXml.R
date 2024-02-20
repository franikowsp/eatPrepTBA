list2Xml <- function(x) {
  x %>%
    purrr::modify_tree(pre = handleXml)
}

handleXml <- function(x) {
  el_names <- names(x)
  el_types <- x %>% purrr::map_chr(typeof)

  if (!is.null(el_names)) {
    empty_names <- el_names == ""
    is_list <- el_types == "list"

    # has_children <- ! empty_names & is_list

    if (any(empty_names) | any(is_list)) {
      child <- x[empty_names | is_list]
    } else {
      child <- list()
    }

    old_names <- attributes(x)
    attributes(child) <- c(x[!(empty_names | is_list)], old_names[!empty_names & is_list])
  } else {
    child <- x
  }

  child
}
