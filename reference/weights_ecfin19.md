# Trade weights of the European Commission

Double export weights from DG ECFIN's price and cost competitiveness
data, used to weight a country's peers into one relative figure. The
number in the name is the size of the peer group the weights are
normalised over: the euro area 19 and 20, the EU 27, and the industrial
country groups of 37 and 42. The weights of one base country in one year
sum to one.

## Usage

``` r
weights_ecfin19

weights_ecfin20

weights_ecfin27

weights_ecfin37

weights_ecfin42
```

## Format

A data frame with four columns:

- geo_base:

  The country whose peers are being weighted.

- time:

  Year.

- geo:

  The peer country the weight belongs to.

- weight:

  Share of the peer group, summing to one over `geo` within each
  `geo_base` and `time`. A country's weight on itself is zero.

An object of class `data.frame` with 28380 rows and 4 columns.

An object of class `data.frame` with 39204 rows and 4 columns.

An object of class `data.frame` with 54945 rows and 4 columns.

An object of class `data.frame` with 62370 rows and 4 columns.

## Source

data-raw/get_ecfin_weights.R,
<https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/price-and-cost-competitiveness/price-and-cost-competitiveness-data-section_en>

## See also

[`weight_index()`](https://tuottavuuslautakunta.github.io/fiprod/reference/weight_index.md),
[`weight_index2()`](https://tuottavuuslautakunta.github.io/fiprod/reference/weight_index.md),
[`read_ecfin_weights()`](https://tuottavuuslautakunta.github.io/fiprod/reference/read_ecfin_weights.md)
