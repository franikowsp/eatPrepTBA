# Generate API request function

This function returns the base API request for a an instance.

## Usage

``` r
generate_base_req(
  type,
  base_url,
  auth_token,
  app_version = NULL,
  insecure = FALSE
)
```

## Arguments

- type:

  Character. Type of the URL request, e.g., `GET` or `POST`.

- base_url:

  Character. Base URL of the instance.

- auth_token:

  Character. Token to interact with the instance API.

- app_version:

  Character. Version of the IQB Studio Lite (not necessary for the IQB
  Testcenter).

- insecure:

  Logical. Should the https security certificate be ignored (only
  recommended for Intranet requests that might not have a valid security
  certificate).

## Value

A `function` with arguments `method`, `endpoint`, and `query`.
