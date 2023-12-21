#' Get multiple units with resources
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve the unit and item metadata from the API.
#' @param units Tibble holding units retrieved from get units.
#'
#' @description
#' This function returns the unit information for multiple units by repeatedly calling [getUnit()].
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' @aliases
#' getUnits,WorkspaceStudio-method
setGeneric("addMetadata", function(workspace, units) {
  standardGeneric("addMetadata")
})

#' @describeIn getUnits Get multiple unit information and coding schemes in a defined workspace
setMethod("addMetadata",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, units) {
            ws_settings <- getSettings(workspace, metadata = TRUE)
            items_profile <- ws_settings %>% purrr::pluck("itemMetadata", 1)
            unit_profile <- ws_settings %>% purrr::pluck("unitMetadata", 1)

            unit_start <-
              units %>%
              dplyr::select(
                -c(unit_profiles, items_profiles)
              ) %>%
              tidyr::unnest(
                items_meta
              )

            items_meta <-
              units %>%
              dplyr::select(unitname, items_profiles) %>%
              tidyr::unnest(items_profiles) %>%
              addProfile(c(unitname, item), profile = items_profile)

            unit_meta <-
              units %>%
              dplyr::select(unitname, unit_profiles) %>%
              tidyr::unnest(unit_profiles) %>%
              addProfile(unitname, profile = unit_profile)

            # ws_info <-
            #   ws_settings %>%
            #   dplyr::select(
            #     ws_id, ws_name, ws_groupId, ws_groupName
            #   )

            unit_start %>%
              # dplyr::bind_cols(ws_info) %>%
              dplyr::left_join(unit_meta) %>%
              dplyr::left_join(items_meta)
          })

# Adds profiles to all columns if possible
addProfile <- function(metadata, id, profile) {
  profile_multiples <-
    profile %>%
    dplyr::filter(multiple) %>%
    dplyr::pull(profile_name)

  profile_vocabularies <-
    profile %>%
    dplyr::filter(profile_type == "vocabulary") %>%
    dplyr::pull(profile_name)

  metadata %>%
    dplyr::mutate(
      value = ifelse(is.na(value_id), value_text, value_id)
    ) %>%
    dplyr::select(
      {{id}}, profile_name, value
    ) %>%
    tidyr::pivot_wider(
      names_from = profile_name,
      values_from = value,
      values_fn = list
    ) %>%
    tidyr::unnest(!tidyr::any_of(profile_multiples)) %>%
    dplyr::mutate(
      dplyr::across(everything(), function(x) {
        col_name <- dplyr::cur_column()

        if (col_name %in% profile_vocabularies) {
          recode_profile <-
            profile %>%
            dplyr::filter(profile_name == col_name) %>%
            purrr::pluck("data", 1)

          if (col_name %in% profile_multiples) {
            purrr::map(x, function(x) factor(x, levels = recode_profile$value_id, labels = recode_profile$value_label))
          } else {
            factor(x, levels = recode_profile$value_id, labels = recode_profile$value_label)
          }
        }
        else {
          x
        }
      })
    )
}
