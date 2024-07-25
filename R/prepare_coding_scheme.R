#' Prepares a readable version of a coding scheme of one unit
#'
#' @description
#' Extends the given coding scheme of a unit to a full data frame that can be filtered for batch checks.
#'
#'
#' @param coding_scheme Coding scheme as prepared by [get_units()] with setting the argument `coding_scheme = TRUE`.
#'
#' @return A tibble.
#' @export
prepare_coding_scheme <- function(coding_scheme) {
  prepared_scheme <-
    coding_scheme %>%
    purrr::pluck("variableCodings") %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      variable_id = "id",
      variable_alias = "alias",
      # TODO: This will replace the page identifier for marker items and derived variables
      variable_label = "label",
      source_type = "sourceType",
      source_parameters = "sourceParameters",
      derive_sources = "deriveSources",
      processing = "processing",
      fragmenting = "fragmenting",
      general_instruction = "manualInstruction",
      # value_array_position = "valueArrayPos",
      # code_model_parameters = "code_model"
      code_model = "codeModel",
      page = "page",
      codes = "codes"
    ))) %>%
    dplyr::mutate(
      codes = purrr::map(codes, prepare_codes)
    ) %>%
    tidyr::unnest(codes, keep_empty = TRUE)

  # TODO: Rework page manipulation
  if (tibble::has_name(prepared_scheme, "pages")) {
    prepared_scheme <-
      prepared_scheme %>%
      dplyr::mutate(
        page = purrr::map(page, function(page) {
          if (is.list(page)) page else list(page)
        })
      )
  }

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
        purrr::list_transpose() %>%
        tibble::as_tibble() %>%
        dplyr::rename(any_of(c(
          rule_operator_and = "ruleOperatorAnd"
        )))
    }
  } else {
    if (!is.null(rule_sets$rules)) {
      tibble::tibble()
    }
  }
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

