# Defaults for saving report figures

The settings
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
uses when it is not told otherwise: the folder, the year that names the
subfolder, and the size, resolution and format of the file.

## Usage

``` r
set_fig_defaults(...)

fig_defaults()
```

## Arguments

- ...:

  Named settings to change: `dir`, `year`, `width`, `height`, `units`,
  `dpi` or `device`. `year = NULL` means the current year.

## Value

`set_fig_defaults()` returns the previous settings invisibly, so they
can be restored. `fig_defaults()` returns the settings in force.

## See also

[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)

## Examples

``` r
fig_defaults()
#> $dir
#> [1] "figures"
#> 
#> $year
#> NULL
#> 
#> $width
#> [1] 13.5
#> 
#> $height
#> [1] 8.5
#> 
#> $units
#> [1] "cm"
#> 
#> $dpi
#> [1] 300
#> 
#> $device
#> [1] "png"
#> 

old <- set_fig_defaults(dir = "kuviot", year = 2026)
fig_defaults()$dir
#> [1] "kuviot"

set_fig_defaults(!!!old)
```
