# Fetch a dataset from the OECD SDMX API

Builds an OECD "Data Explorer" REST data request directly
(`https://sdmx.oecd.org/public/rest/data/...`), reads the response with
`xml2` and flattens its SDMX-ML Generic data into a plain data frame.
Used in place of the `OECD` package (`github::expersso/OECD`, not on
CRAN and not kept up with this newer API) and, as of this version,
without `rsdmx` either: its own bundled `"OECD"` service provider
profile still points at OECD's older `stats.oecd.org` warehouse, which
does not know the dataflow ids (such as `"DSD_PDB@DF_PDB"`) this
package's `data-raw/get_oecd_*.R` scripts already use, and using it only
as a generic SDMX-ML reader (as an earlier version of this function did)
was one more dependency for what `xml2` alone already does.

## Usage

``` r
oecd_get_dataset(
  dataset,
  filter,
  start = NULL,
  end = NULL,
  base_url = "https://sdmx.oecd.org/public/rest",
  connecttimeout = 30,
  timeout = 180
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

- connecttimeout, timeout:

  Connection and total request time limits (in seconds), passed straight
  to [`new_handle`](https://rdrr.io/pkg/curl/man/handle.html). Plain
  `xml2::read_xml(url)` opens the connection through `curl` without any
  time limit of its own, so a stalled server (or a network path that
  accepts the connection but never finishes sending data) would
  otherwise hang indefinitely instead of raising an error.

## Value

A data frame of the raw SDMX observations.

## Details

Returns a plain data frame, shaped like
[`oecd_clean_data`](https://tuottavuuslautakunta.github.io/fiprod/reference/oecd_clean_data.md)
expects: one row per observation, a column per SDMX dimension and series
or observation level attribute, `obsTime` and `obsValue`.

## See also

[`oecd_clean_data`](https://tuottavuuslautakunta.github.io/fiprod/reference/oecd_clean_data.md)
to reshape the result,
[`oecd_make_filter`](https://tuottavuuslautakunta.github.io/fiprod/reference/oecd_make_filter.md)
to build `filter`.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- oecd_get_dataset(
  dataset = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
  filter  = oecd_make_filter(list(c("FIN", "USA"), "A", "GVAHRS"))
)
} # }
```
