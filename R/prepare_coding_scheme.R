#' Prepares a readable version of a coding scheme of one unit
#'
#' @description
#' Extends the given coding scheme of a unit to a full data frame that can be filtered for batch checks.
#'
#'
#' @param coding_scheme Coding scheme as prepared by [get_units()] with setting the argument `coding_scheme = TRUE`.
#' @param filter_has_codes Only returns variables that were not deactivated. Defaults to `TRUE`.
#'
#' @return A tibble.
#' @export
prepare_coding_scheme <- function(coding_scheme, filter_has_codes = TRUE) {
  if (is.null(coding_scheme)) {
    return(tibble::tibble())
  }

  scheme_table <-
    coding_scheme %>%
    purrr::pluck("variableCodings") %>%
    purrr::list_transpose() %>%
    tibble::as_tibble()

  # For legacy reasons, this has to be added
  # TODO: Can this be removed at a later point in time?
  if (tibble::has_name(scheme_table, "alias")) {
    unit_cols <- c(
      variable_ref = "id",
      variable_id = "alias"
    )

    scheme_table <-
      scheme_table %>%
      tidyr::unnest(alias, keep_empty = TRUE) %>%
      dplyr::mutate(
        alias = ifelse(is.na(alias), id, alias)
      )
  } else {
    unit_cols <- c(
      variable_id = "id"
    )
  }

  # Level of variable in dependency tree (makes it easier to search for dependencies)
  sources <-
    coding_scheme %>%
    eatAutoCode::get_dependency_tree() %>%
    tibble::as_tibble() %>%
    dplyr::select(
      variable_id = "id",
      variable_level = "level"
    )

  prepared_scheme <-
    scheme_table %>%
    dplyr::rename(any_of(c(
      unit_cols,
      # TODO: This might replace the page identifier for marker items and derived variables
      variable_label = "label",
      source_type = "sourceType",
      source_parameters = "sourceParameters",
      derive_sources = "deriveSources",
      processing = "processing",
      fragmenting = "fragmenting",
      general_instruction = "manualInstruction",
      code_model = "codeModel",
      page = "page",
      codes = "codes"
    ))) %>%
    dplyr::filter(dplyr::if_any(c("source_type"), function(x) {
      if (filter_has_codes) {
        x != "BASE_NO_VALUE"
      } else TRUE
    })) %>%
    dplyr::left_join(sources, by = dplyr::join_by("variable_id")) %>%
    dplyr::mutate(
      codes = purrr::map(codes, prepare_codes)
    ) %>%
    tidyr::unnest(codes, keep_empty = TRUE) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("fragmenting")),
                    list_to_character),
      dplyr::across(dplyr::any_of(c("page")),
                    list_to_integer)
    )

  if (tibble::has_name(prepared_scheme, "rule_sets")) {
    prepared_rule_sets <-
      prepared_scheme %>%
      dplyr::mutate(
        rule_sets = purrr::map(rule_sets, prepare_rule_sets)
      ) %>%
      tidyr::unnest(rule_sets, keep_empty = TRUE)

    if (tibble::has_name(prepared_rule_sets, "rules")) {
      prepared_rule_sets %>%
        dplyr::mutate(
          rules = purrr::map(rules, prepare_rules)
        ) %>%
        tidyr::unnest(rules, keep_empty = TRUE)
    } else {
      prepared_rule_sets
    }
  } else {
    prepared_scheme
  }
}

prepare_codes <- function(codes) {
  codes %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      code_id = "id",
      code_label = "label",
      code_score = "score",
      rule_set_operator_and = "ruleSetOperatorAnd",
      rule_sets = "ruleSets",
      code_manual_instruction = "manualInstruction",
      code_type = "type"
    ))
    )
}

prepare_rule_sets <- function(rule_sets) {
  if (!is.null(rule_sets)) {
    if (is.null(names(rule_sets)) & length(rule_sets) > 0) {
      rule_sets %>%
        purrr::map(function(x) x %>%
                     tibble::as_tibble() %>%
                     dplyr::mutate(
                       dplyr::across(dplyr::any_of(c("valueArrayPos")),
                                     list_to_character)
                     )
        ) %>%
        # purrr::list_transpose() %>%
        # tibble::as_tibble() %>%
        dplyr::bind_rows() %>%
        dplyr::rename(any_of(c(
          rule_operator_and = "ruleOperatorAnd",
          value_array_position = "valueArrayPos"
        )))
    }
  } else {
    if (!is.null(rule_sets$rules)) {
      tibble::tibble()
    }
  }
}

list_to_character <- function(x) {
  purrr::map_chr(x, function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }

    as.character(x)
  })
}

list_to_integer <- function(x) {
  purrr::map_int(x, function(x) {
    if (is.null(x)) {
      return(NA_integer_)
    }

    as.integer(x)
  })
}

coerce_list <- function(x) {
  if (is.list(x)) x else list(x)
}

prepare_rules <- function(rules) {
  if (is.null(rules) || length(rules) == 0) {
    prepared_rules <-
      tibble::tibble()
  } else if (is.null(names(rules)) & length(rules) >= 1) {
    prepared_rules <-
      rules %>%
      purrr::list_transpose() %>%
      tibble::as_tibble()
  } else {
    prepared_rules <-
      rules %>%
      tibble::as_tibble()
  }

  if (tibble::has_name(rules, "parameter")) {
    prepared_rules %>%
      tidyr::unnest(parameters, keep_empty = TRUE)
  } else {
    prepared_rules
  }
}

