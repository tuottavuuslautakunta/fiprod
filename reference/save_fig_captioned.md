# Save a standalone, captioned report figure

Writes a second version of a report figure to
`<dir>/<year>/<name>.<device>`, wider than
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)'s
and with `title`, `subtitle` and `caption` drawn on the plot itself (via
[`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)),
for uses where the figure travels on its own, without the running text
that carries the title, subtitle and source next to
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)'s
output. `dir` defaults to a sibling of
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)'s
own folder (`figures/otsikoilla/<year>/` next to `figures/<year>/`), so
the two never collide, and the file name is normally the same as the one
passed to
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
for the same plot.

## Usage

``` r
save_fig_captioned(plot, name, title = NULL, subtitle = NULL, caption = NULL, ...)
```

## Arguments

- plot:

  A plot object, normally a `ggplot`.

- name:

  File name without the extension, normally the same name used for the
  same plot's
  [`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
  call.

- title, subtitle, caption:

  Text for
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).
  `NULL` leaves that label unset.

- ...:

  Settings for this call only, overriding
  [`fig_captioned_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_captioned_defaults.md):
  `dir`, `year`, `width`, `height`, `units`, `dpi`, `device`,
  `title_size`, `subtitle_size`, `text_size`, `caption_size`,
  `title_wrap`, `subtitle_wrap`, `caption_wrap`.

## Value

`plot`, so that the figure is still drawn.

## Details

A chunk that already ends in
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
gets the captioned version with one more line:

    p <- dat |> ggplot2::ggplot(...)
    save_fig(p, "bkt-per-capita")
    save_fig_captioned(p, "bkt-per-capita",
      title = "BKT per capita",
      subtitle = "Vuoden 2020 $ hinnoin ostovoimakorjattuna",
      caption = "Lähde: Eurostat, OECD, Tuottavuuslautakunta.")

Blanking the axis titles and legend title with `the_title_blank("xyl")`
(as the report's figures do) leaves the plot title, subtitle and caption
alone, so the same plot can be passed to both functions without change.

The figure is bigger than
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)'s,
so its text is set bigger too, the title by the most: by default the
title grows from about 12pt to 20pt (a plot built at the report's usual
`theme_fpb(base_size = 11)`), against a more modest lift for the axis
text, legend text and facet strip text (to 12pt) and the caption (to
10pt). A figure's own `theme()` overrides for these elements (a smaller
legend text to fit a long legend, say) are replaced along with the rest,
so every captioned figure ends up with the same text sizes regardless of
what the small report figure needed.

Each of `title`, `subtitle` and `caption` is wrapped to its own line
width (`title_wrap`, `subtitle_wrap`, `caption_wrap`) before being set,
so that growing the text (or narrowing the figure) doesn't just run it
off the edge; the defaults are sized for the default
`title_size`/`subtitle_size`/ `caption_size` at the default `width`, so
a call that changes one of those should normally change the matching
`_wrap` setting too.

## See also

[`set_fig_captioned_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_captioned_defaults.md)
to change the defaults for every captioned figure at once;
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
for the plain report figure.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
save_fig_captioned(p, "wt-mpg", dir = tempdir(),
  title = "Auton paino ja polttoaineenkulutus",
  subtitle = "Mailia gallonalla painon (1000 lbs) mukaan",
  caption = "Lähde: mtcars")
} # }
```
