# Save a report figure

Writes a figure to `<dir>/<year>/<name>.<device>`, creating the folder
if it is not there. The year is the year of publication, so that the
figures of successive reports stay side by side instead of overwriting
each other.

## Usage

``` r
save_fig(plot, name, ...)
```

## Arguments

- plot:

  A plot object, normally a `ggplot`.

- name:

  File name without the extension.

- ...:

  Settings for this call only, overriding
  [`fig_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_defaults.md):
  `dir`, `year`, `width`, `height`, `units`, `dpi`, `device`.

## Value

`plot`, so that the figure is still drawn.

## Details

The plot is returned, so a chunk that ends in `save_fig()` both writes
the file and shows the figure:

    dat |>
      ggplot2::ggplot(...) |>
      save_fig("bkt-per-capita")

## See also

[`set_fig_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_defaults.md)
to change the defaults for every figure at once.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
save_fig(p, "wt-mpg", dir = tempdir())
} # }
```
