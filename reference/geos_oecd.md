# Countries of the OECD productivity database query

The countries fetched from the OECD productivity database, as ISO three
letter codes, with `EA20` for the euro area aggregate. The three letter
codes are what the OECD API wants; they are turned into Eurostat codes
when the data is read.

## Usage

``` r
geos_oecd
```

## Format

A character vector of length 16.

## Source

data-raw/data_main.R, used by data-raw/get_oecd_pdb.R
