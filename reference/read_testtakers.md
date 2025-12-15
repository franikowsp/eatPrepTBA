# Reads a testtakers XML

Reads a testtakers XML

## Usage

``` r
read_testtakers(testtakers_xml)
```

## Arguments

- testtakers_xml:

  Must be a testtakers XML document.

## Value

A tibble.

## Details

Please note that this currently only works if you have a structure of
`Group > Login > Booklet` in your testtakers XML.
