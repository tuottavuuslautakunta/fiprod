# Combine data sources country by country

Picks each country from the first source that has it. The OECD
productivity database is updated more slowly than Eurostat, so the EU
(and EEA) countries are normally taken from Eurostat and the rest from
the OECD.

## Usage

``` r
combine_geo_sources(..., geos = NULL, geo = "geo", source_col = "source")
```

## Arguments

- ...:

  Named data frames in priority order, e.g.
  `combine_geo_sources(eurostat = dat_eurostat, oecd = dat_oecd)`. Only
  the columns shared by all sources are kept.

- geos:

  Optional named list restricting which countries are taken from which
  source, e.g. `list(eurostat = geo_ea)`. Sources without an entry
  contribute every country that no earlier source provided.

- geo:

  Name of the country column.

- source_col:

  Name of a column recording which source each row came from, or `NULL`
  to leave it out.

## Value

A data frame with the rows of all sources, each country from one source
only.

## See also

[`compare_sources()`](https://tuottavuuslautakunta.github.io/fiprod/reference/compare_sources.md)
to check that the sources are on the same scale before combining them.

## Examples

``` r
eurostat <- tibble::tibble(geo = c("FI", "SE"), time = 2020, values = 1:2)
oecd     <- tibble::tibble(geo = c("FI", "US"), time = 2020, values = 3:4)

# FI comes from Eurostat, US from the OECD
combine_geo_sources(eurostat = eurostat, oecd = oecd)
#> # A tibble: 3 × 4
#>   geo    time values source  
#>   <chr> <dbl>  <int> <chr>   
#> 1 FI     2020      1 eurostat
#> 2 SE     2020      2 eurostat
#> 3 US     2020      4 oecd    
```
