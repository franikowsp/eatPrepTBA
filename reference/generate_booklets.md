# Generates booklet XMLs from booklet, testlet, and unit information

Please note that the function currently only works for units that are
nested within

## Usage

``` r
generate_booklets(booklets, app_version = "16.0.2", login = NULL)
```

## Arguments

- booklets:

  Must be a tibble with the columns `booklet_id`, `booklet_label`, and
  `booklet_units`.Optionally, the columns `booklet_description`
  (character) and `booklet_configuration` (list) can be added. The
  (list) column `booklet_units` is a nested tibble with columns
  `testlet_id`, `testlet_label`, and `units`. Optionally, it can contain
  the column `testlet_restrictions`. Finally, the (list) column `units`
  is again a nested tibble with columns `unit_key`, `unit_alias`,
  `unit_label`, and `unit_labelshort`.

- app_version:

  Version of the target Testcenter instance. Defaults to `"16.0.0"`.

- login:

  Target Testcenter instance. If it is available, the `app_version` will
  be overwritten.

## Value

A booklet XML.
