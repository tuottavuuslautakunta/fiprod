# Defaults for saving a report figure's data

The settings
[`save_fig_data()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_data.md)
uses when it is not told otherwise: the folder and the year that names
the subfolder.

## Usage

``` r
set_fig_data_defaults(...)

fig_data_defaults()
```

## Arguments

- ...:

  Named settings to change: `dir` or `year`. `year = NULL` means the
  current year.

## Value

`set_fig_data_defaults()` returns the previous settings invisibly, so
they can be restored. `fig_data_defaults()` returns the settings in
force.

## See also

[`save_fig_data()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_data.md)

## Examples

``` r
fig_data_defaults()
#> $dir
#> [1] "figures/data"
#> 
#> $year
#> NULL
#> 

old <- set_fig_data_defaults(dir = "data", year = 2026)
fig_data_defaults()$dir
#> [1] "data"

set_fig_data_defaults(!!!old)
```
