# # Bring it all together
# items_part2 %>%
#   left_join(items_part1) %>%
#   left_join(item_profile_read_select)
#
#
# unit_profile <- jsonlite::read_json("https://raw.githubusercontent.com/iqb-vocabs/p11/master/unit.json")
#
# unit_profile_prep <-
#   unit_profile %>%
#   pluck("groups", 1, "entries") %>%
#   map(function(x) unlist(x) %>% as.list() %>% as_tibble()) %>%
#   reduce(bind_rows)
#
# unit_profile_read <-
#   unit_profile_prep %>%
#   select(profile_id = id, label.value, type, parameters.url) %>%
#   mutate(
#     profile = map(parameters.url, master_list)
#   ) %>%
#   unnest(profile)
#
# unit %>%
#   left_join(unit_profile_read %>%   select(
#     label.value,
#     value.id = id,
#     label = prefLabel.de,
#     notation = notation
#   )) %>%
#   mutate(
#     value = ifelse(is.na(label), valueAsText.value, label)
#   ) %>%
#   select(
#     name = label.value,
#     value = value
#   ) %>%
#   pivot_wider() %>%
#   reactable::reactable(style = list(fontFamily = "Open Sans"),
#                        columns = reactable::colDef(style = list(fontFamily = "Lucida Console")))
#
#
#
#
#
#
