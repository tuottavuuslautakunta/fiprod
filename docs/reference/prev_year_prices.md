# Convert between fixed and previous year's price series

Chain linked volume series cannot be added together over industries (or
any other breakdown), because every series carries its own chain of
price structures. Series in previous year's prices (PYP) can be added,
since within a year all of them are valued at the same, previous year's,
price level. Aggregating a volume series therefore takes three steps:
convert the components to previous year's prices, add them up, and chain
the sum back to a fixed price series.

## Usage

``` r
prev_year_prices(cp, fp, time)

fixed_prices(cp, pyp, time, ref_year = 2020)
```

## Arguments

- cp:

  A numeric vector of current price values.

- fp:

  A numeric vector of fixed price (chain linked volume) values.

- time:

  A vector of dates or years. One observation per year is required, but
  the years need not be sorted or consecutive.

- pyp:

  A numeric vector of previous year's price values.

- ref_year:

  Numeric reference (base) year of the returned fixed price series. Must
  be present in `time`.

## Value

A numeric vector as long as `time`.

## Details

`prev_year_prices()` does the first step and `fixed_prices()` the last
one:

\$\$pyp_t = cp\_{t-1} \times fp_t / fp\_{t-1}\$\$ \$\$fp_t = fp\_{t-1}
\times pyp_t / cp\_{t-1}\$\$

`fixed_prices()` chains outwards from `ref_year` in both directions, so
the result equals `cp` in the reference year. The chain stops at a break
(a missing `cp` or `pyp`, or a gap in `time`) and all later (earlier)
years are `NA`, because a volume series cannot be linked across a break.

The functions are yearly-data equivalents of
[`statfitools::pp()`](https://rdrr.io/pkg/statfitools/man/pp.html) and
[`statfitools::fp()`](https://rdrr.io/pkg/statfitools/man/fp.html).

## Examples

``` r
cp   <- c(100, 120, 150)
fp20 <- c(90, 110, 150)
time <- 2018:2020

pyp <- prev_year_prices(cp, fp20, time)
pyp
#> [1]       NA 122.2222 163.6364

# Chaining back reproduces the original volume series
fixed_prices(cp, pyp, time, ref_year = 2020)
#> [1]  90 110 150
```
