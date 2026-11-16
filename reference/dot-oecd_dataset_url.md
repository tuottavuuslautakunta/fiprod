# The OECD SDMX REST request URL for a dataset and filter

Split out of
[`oecd_get_dataset()`](https://tuottavuuslautakunta.github.io/fiprod/reference/oecd_get_dataset.md)
so the URL it builds can be tested without a network call.

## Usage

``` r
.oecd_dataset_url(
  dataset,
  filter,
  start = NULL,
  end = NULL,
  base_url = "https://sdmx.oecd.org/public/rest"
)
```

## Arguments

- dataset:

  The dataflow identifier, exactly as it already appears in the
  `data-raw/get_oecd_*.R` scripts, e.g. `"OECD.SDD.TPS,DSD_PDB@DF_PDB,"`
  (agency, dataflow id and version, comma separated; an empty version
  means "latest"). Used as-is as the request URL's dataflow path
  segment.

- filter:

  The dimension filter, built by
  [`oecd_make_filter`](https://tuottavuuslautakunta.github.io/fiprod/reference/oecd_make_filter.md)
  (a dot separated string, with `+` joining several codes within one
  dimension). Used as-is as the request URL's key path segment.

- start, end:

  First/last period to fetch (a year, for annual data), sent as the
  request's `startPeriod`/`endPeriod`. `NULL` (the default) fetches
  every period, as the existing `get_oecd_*.R` scripts do.

- base_url:

  The OECD SDMX REST API's base URL. Exposed for testing; there should
  be no reason to change it otherwise.

## Value

The request URL, a single string.
