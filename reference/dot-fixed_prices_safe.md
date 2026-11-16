# Chain a fixed price series, returning NA instead of failing

Chain a fixed price series, returning NA instead of failing

## Usage

``` r
.fixed_prices_safe(cp, pyp, time, ref_year, label = NULL)
```

## Arguments

- cp:

  A numeric vector of current price values.

- pyp:

  A numeric vector of previous year's price values.

- time:

  A vector of dates or years. One observation per year is required, but
  the years need not be sorted or consecutive.

- ref_year:

  Numeric reference (base) year of the returned fixed price series. Must
  be present in `time`.

- label:

  A label used in the warning message.
