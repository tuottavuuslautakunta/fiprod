# Annual exchange rates against the euro

National currency per euro, yearly averages from Eurostat, from 1971
onwards. The euro area countries carry a rate of 1 for `EUR`, so that a
conversion works the same way for every country.

## Usage

``` r
exh_eur_a
```

## Format

A tibble with four columns:

- time:

  Year, as a date on the first of January.

- currency:

  Currency code, e.g. `SEK`.

- values:

  Units of the currency per euro.

- geo:

  Country the currency belongs to, as a Eurostat code.

## Source

data-raw/get_exch.R, Eurostat

## See also

[`convert_currency()`](https://tuottavuuslautakunta.github.io/fiprod/reference/convert_currency.md)
