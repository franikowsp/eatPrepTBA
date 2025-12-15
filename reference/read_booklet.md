# Reads a booklet XML

Reads a booklet XML

## Usage

``` r
read_booklet(booklet_xml)
```

## Arguments

- booklet_xml:

  Must be a booklet XML document.

## Value

A tibble.

## Details

Please note that this currently only works if you have a structure of
`Units > Testlet > Unit` or `Units > Unit` in your booklet XML. Further
nesting is currently not supported. Moreover, it only extracts the unit
order.
