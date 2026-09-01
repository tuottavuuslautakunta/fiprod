# Unit labour cost index

Nominal unit labour costs are labour cost per unit of output: the
compensation of employees divided by the volume of output. Both sides
can be put on a per head or per hour basis by giving the labour inputs.

## Usage

``` r
ind_ulc(cost, output, input1 = 1, input2 = 1, time, baseyear)
```

## Arguments

- cost:

  Compensation of employees, current prices.

- output:

  Value added or GDP, chain linked volume.

- input1:

  Labour input of the cost, e.g. employees. Defaults to 1, which leaves
  the cost as a total.

- input2:

  Labour input of the output, e.g. employed persons. Defaults to 1.

- time:

  A vector of dates or years.

- baseyear:

  Year or years the index is set to 100 in.

## Value

A numeric vector, an index with `baseyear` at 100.

## Details

The entrepreneur adjusted measure that the productivity board reports
uses compensation per employee (`input1 = employees`) against output per
employed person (`input2 = employed`). It scales the cost of employees
up to all of the labour used, which matters in countries where the
self-employed are a large share of employment.

Dividing the compensation in euro rather than in national currency gives
the common currency measure, which moves with the exchange rate as well.

## See also

[`gdp_trading_gain()`](https://tuottavuuslautakunta.github.io/fiprod/reference/gdp_trading_gain.md)
for the terms of trade adjusted output,
[`rebase_index()`](https://tuottavuuslautakunta.github.io/fiprod/reference/rebase_index.md),
[`weight_index2()`](https://tuottavuuslautakunta.github.io/fiprod/reference/weight_index.md)
for the weighting against peers.

## Examples

``` r
cost   <- c(100, 104, 110)
output <- c(200, 205, 208)
time   <- 2018:2020

ind_ulc(cost, output, time = time, baseyear = 2020)
#> [1]  94.54545  95.92905 100.00000

# per employee against output per employed person
ind_ulc(cost, output, input1 = c(50, 50, 51), input2 = c(60, 60, 61),
        time = time, baseyear = 2020)
#> [1]  94.85544  96.24357 100.00000
```
