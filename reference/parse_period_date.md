# Parse a year or year-quarter period column into a `time` Date column

A small stand-in for `pxwebtools::parse_dates()`, covering only the two
period formats this package's own code needs: `"Y"` (a 4-digit year,
e.g. `"2021"`) and `"y-q"` (year and quarter, e.g. `"2021-Q1"`, as
OECD's quarterly data uses), each mapped to the first day of that year
or quarter. As with `parse_dates()`, the parsed column always ends up
named `time`, replacing whatever column `date_format`'s name pointed at.

## Usage

``` r
parse_period_date(df, date_format)
```

## Arguments

- df:

  A data frame.

- date_format:

  A single named character vector, `c(<column> = "Y")` or
  `c(<column> = "y-q")`: the column to parse and which format it is in.

## Value

`df`, with `date_format`'s column parsed into a `Date` and renamed to
`time`.

## Examples

``` r
parse_period_date(data.frame(year = c("2020", "2021")), c(year = "Y"))
#>         time
#> 1 2020-01-01
#> 2 2021-01-01
parse_period_date(data.frame(obsTime = c("2021-Q1", "2021-Q2")),
                  c(obsTime = "y-q"))
#>         time
#> 1 2021-01-01
#> 2 2021-04-01
```
