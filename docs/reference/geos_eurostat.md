# Countries taken from Eurostat when the sources are combined

The OECD productivity database lags Eurostat by several months, so these
countries are taken from Eurostat and the rest from the OECD. Everything
outside this vector — the US, Japan and the United Kingdom, which
Eurostat no longer updates — comes from the OECD.

## Usage

``` r
geos_eurostat
```

## Format

A character vector of length 24: `EA20`, the euro area countries,
Sweden, Denmark and Norway.

## Source

data-raw/data_main.R

## See also

[`combine_geo_sources()`](https://tuottavuuslautakunta.github.io/fiprod/reference/combine_geo_sources.md)
