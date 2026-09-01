# Compare two data sources on their overlap

Reports how two sources relate on the observations they share. Use it
before
[`combine_geo_sources()`](https://tuottavuuslautakunta.github.io/fiprod/reference/combine_geo_sources.md)
to check that the series are on the same scale (a median ratio of e.g.
1000 means one source is in millions and the other in thousands) and
that they measure the same thing (a growth correlation well below one
means they do not).

## Usage

``` r
compare_sources(
  x,
  y,
  by = c("geo", "activity", "measure", "price_base"),
  time = "time",
  values = "values"
)
```

## Arguments

- x, y:

  Data frames to compare.

- by:

  Character vector of columns identifying a series.

- time:

  Name of the time column.

- values:

  Name of the value column.

## Value

A data frame with one row per series: the number of shared observations
`n`, the median, smallest and largest ratio of `x` to `y`, and
`growth_cor`, the correlation of the yearly relative changes.

## Examples

``` r
x <- tibble::tibble(geo = "FI", time = 2018:2020, values = c(100, 110, 120))
y <- tibble::tibble(geo = "FI", time = 2018:2020, values = c(0.1, 0.11, 0.12))
compare_sources(x, y, by = "geo")
#> # A tibble: 1 × 6
#>   geo       n ratio_median ratio_min ratio_max growth_cor
#>   <chr> <int>        <dbl>     <dbl>     <dbl>      <dbl>
#> 1 FI        3         1000      1000      1000          1
```
