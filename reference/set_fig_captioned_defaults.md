# Defaults for saving standalone, captioned report figures

The settings
[`save_fig_captioned()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_captioned.md)
uses when it is not told otherwise: the folder, the year that names the
subfolder, the size and resolution of the file, the point size of the
title, subtitle, axis/legend/strip text and caption, and the line width
each of the title, subtitle and caption is wrapped at.

## Usage

``` r
set_fig_captioned_defaults(...)

fig_captioned_defaults()
```

## Arguments

- ...:

  Named settings to change: `dir`, `year`, `width`, `height`, `units`,
  `dpi`, `device`, `title_size`, `subtitle_size`, `text_size`,
  `caption_size`, `title_wrap`, `subtitle_wrap` or `caption_wrap`.
  `year = NULL` means the current year; any `_size` or `_wrap` set to
  `NULL` leaves that element as the plot already has it (for a `_size`)
  or turns off its wrapping (for a `_wrap`).

## Value

`set_fig_captioned_defaults()` returns the previous settings invisibly,
so they can be restored. `fig_captioned_defaults()` returns the settings
in force.

## See also

[`save_fig_captioned()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_captioned.md)

## Examples

``` r
fig_captioned_defaults()
#> $dir
#> [1] "figures/otsikoilla"
#> 
#> $year
#> NULL
#> 
#> $width
#> [1] 16
#> 
#> $height
#> [1] 12
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
#> $title_size
#> [1] 20
#> 
#> $subtitle_size
#> [1] 14
#> 
#> $text_size
#> [1] 12
#> 
#> $caption_size
#> [1] 10
#> 
#> $title_wrap
#> [1] 40
#> 
#> $subtitle_wrap
#> [1] 65
#> 
#> $caption_wrap
#> [1] 90
#> 

old <- set_fig_captioned_defaults(dir = "kuviot", year = 2026)
fig_captioned_defaults()$dir
#> [1] "kuviot"

set_fig_captioned_defaults(!!!old)
```
