# Extend a series forward with growth rates from another source

Annual national accounts arrive late for some countries, while a
quarterly or monthly indicator of the same thing is already out. This
carries the level of the annual series forward using the growth of the
timelier one, so a chart does not have to stop a year or two short.

## Usage

``` r
extend_with_change(x, change, time)
```

## Arguments

- x:

  A numeric vector of levels or index values, with the missing tail as
  `NA`.

- change:

  A numeric vector of relative changes aligned to `x`, e.g. `0.03` for
  three per cent. Only the values after the last observation of `x` are
  used.

- time:

  A vector of dates or years used to order the series. The result is
  returned in the order it was given in.

## Value

A numeric vector as long as `x`. `NA` where `x` was missing and `change`
did not reach.

## Details

Only the tail is filled: values are written after the last observation
of `x` and nothing before or between is touched. The result is an
extrapolation, not a measurement, and is worth marking as such wherever
it is shown.

## See also

[`ind_ulc()`](https://tuottavuuslautakunta.github.io/fiprod/reference/ind_ulc.md),
[`rebase_index()`](https://tuottavuuslautakunta.github.io/fiprod/reference/rebase_index.md)

## Examples

``` r
level  <- c(100, 105, 110, NA, NA)
growth <- c(NA, 0.05, 0.048, 0.05, 0.04)
time   <- 2020:2024

extend_with_change(level, growth, time)
#> [1] 100.00 105.00 110.00 115.50 120.12

# 110 * 1.05 and then * 1.04
```
