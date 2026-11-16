# Aggregate national accounts industries

Builds industry aggregates (such as the OECD business sector `BTNXL`,
i.e. NACE B to N excluding L) from a long data frame of national
accounts series.

## Usage

``` r
aggregate_activities(
  df,
  key,
  ref_year = 2020,
  cp = "V",
  fp = "LR",
  pyp = "Y",
  additive = "_Z",
  activity = "activity",
  price_base = "price_base",
  values = "values",
  time = "time",
  append = TRUE
)
```

## Arguments

- df:

  A long data frame with an activity column, a price base column, a time
  column and a numeric value column. All remaining columns (`geo`,
  `measure`, `unit_measure`, ...) identify a series and are kept as they
  are. Columns that encode the price base themselves (such as the
  `var_id` of the OECD tables) must be dropped first; the function stops
  if it finds one.

- key:

  A named list mapping each new activity code to its components, e.g.
  `list(BTNXL = c("BTE", "F", "GTNXL"))`. Components must not overlap
  within one aggregate.

- ref_year:

  Reference (base) year of the chained fixed price series.

- cp, fp, pyp:

  Values of the price base column identifying current price, fixed price
  (chain linked volume) and previous year's price series. Previous
  year's price series are derived from `cp` and `fp` when absent.

- additive:

  Values of the price base column that may be added up as they are, such
  as `"_Z"` for employment and hours. Series that are neither additive
  nor one of `cp`, `fp`, `pyp` (chain linked indices, ratios such as
  value added per hour) cannot be aggregated and are dropped.

- activity, price_base, values, time:

  Column names in `df`.

- append:

  If `TRUE` (default) the aggregates are added to `df`, replacing any
  existing rows for the same activity codes. If `FALSE` only the
  aggregates are returned.

## Value

A data frame with the same columns as `df`.

## Details

Current price and previous year's price series are simply added up.
Chain linked volumes are not additive, so they are converted to previous
year's prices with
[`prev_year_prices()`](https://tuottavuuslautakunta.github.io/fiprod/reference/prev_year_prices.md),
added up, and chained back with
[`fixed_prices()`](https://tuottavuuslautakunta.github.io/fiprod/reference/prev_year_prices.md).
When the data already contains previous year's price series (Eurostat
publishes them, OECD does not) those are used and only the missing ones
are derived from the current and fixed price series.

An aggregate is `NA` in every year in which any of its components is
missing, so that a partial sum is never mistaken for the aggregate.

## See also

[`prev_year_prices()`](https://tuottavuuslautakunta.github.io/fiprod/reference/prev_year_prices.md),
[`combine_geo_sources()`](https://tuottavuuslautakunta.github.io/fiprod/reference/combine_geo_sources.md)

## Examples

``` r
dat <- tibble::tibble(
  geo        = "FI",
  measure    = "GVA",
  activity   = rep(c("F", "J"), each = 6),
  price_base = rep(rep(c("V", "LR"), each = 3), 2),
  time       = rep(2018:2020, 4),
  values     = c(100, 110, 120,  95, 105, 120,   # construction
                  50,  52,  60,  48,  50,  60)   # information
)

aggregate_activities(dat, list(FJ = c("F", "J")), ref_year = 2020)
#> # A tibble: 18 × 6
#>    geo   measure activity price_base  time values
#>    <chr> <chr>   <chr>    <chr>      <int>  <dbl>
#>  1 FI    GVA     F        V           2018   100 
#>  2 FI    GVA     F        V           2019   110 
#>  3 FI    GVA     F        V           2020   120 
#>  4 FI    GVA     F        LR          2018    95 
#>  5 FI    GVA     F        LR          2019   105 
#>  6 FI    GVA     F        LR          2020   120 
#>  7 FI    GVA     J        V           2018    50 
#>  8 FI    GVA     J        V           2019    52 
#>  9 FI    GVA     J        V           2020    60 
#> 10 FI    GVA     J        LR          2018    48 
#> 11 FI    GVA     J        LR          2019    50 
#> 12 FI    GVA     J        LR          2020    60 
#> 13 FI    GVA     FJ       V           2018   150 
#> 14 FI    GVA     FJ       LR          2018   143.
#> 15 FI    GVA     FJ       V           2019   162 
#> 16 FI    GVA     FJ       LR          2019   155.
#> 17 FI    GVA     FJ       V           2020   180 
#> 18 FI    GVA     FJ       LR          2020   180 
```
