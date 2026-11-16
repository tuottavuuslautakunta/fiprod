# Peer group of the price competitiveness indicators

The seventeen countries Finland's relative unit labour costs are
weighted against, as in the old ficomp package. `geos_comp_es` are the
fifteen covered by Eurostat's national accounts, Switzerland included
through EFTA; `geos_comp_oecd` are the two that are not and come from
the OECD productivity database instead. That database carries the unit
labour cost and its parts but no exports or imports, so the terms of
trade adjusted measures exist for the fifteen only.

## Usage

``` r
geos_comp

geos_comp_es

geos_comp_oecd
```

## Format

Character vectors of length 17, 15 and 2.

An object of class `character` of length 15.

An object of class `character` of length 2.

## Source

data-raw/data_main.R

## See also

[`weight_index2()`](https://tuottavuuslautakunta.github.io/fiprod/reference/weight_index.md),
[`ind_ulc()`](https://tuottavuuslautakunta.github.io/fiprod/reference/ind_ulc.md)
