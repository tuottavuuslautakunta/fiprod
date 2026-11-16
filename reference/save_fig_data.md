# Save a report figure's data in wide format

Writes the data behind a report figure to `<dir>/<year>/<name>.xlsx`, so
that the numbers behind a figure can be read or copied in Excel without
the original, longer table the figure was drawn from.

## Usage

``` r
save_fig_data(
  plot,
  name,
  names_from = NULL,
  values_from = "values",
  id_cols = NULL,
  ...
)
```

## Arguments

- plot:

  A plot object (its `$data` is widened and written) or a data frame to
  widen and write directly.

- name:

  File name without the extension, normally the same name used for the
  same figure's
  [`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
  call.

- names_from, values_from, id_cols:

  Passed on to
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html):
  `names_from` is the column (or columns, for more than one grouping
  variable, such as a facet and a colour both) whose values become new
  column names; `values_from` is the column the new columns are filled
  from, `"values"` by default, the name every figure in the report uses;
  `id_cols` are the columns that stay as rows, the rest of the data by
  default. `names_from = NULL` (the default) writes the data as it is,
  without widening, for a figure with a single series and so nothing to
  spread into columns.

- ...:

  Settings for this call only, overriding
  [`fig_data_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_data_defaults.md):
  `dir`, `year`.

## Value

`plot`, unchanged, so that a chunk that still draws the figure can end
in this call.

## Details

A chunk that already ends in
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)/[`save_fig_captioned()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_captioned.md)
gets the data with one more line, reusing the same plot: `plot$data` is
what is widened and written, so nothing about the plot's own code needs
to change.

    p <- dat |> ggplot2::ggplot(ggplot2::aes(time, values, colour = geo)) + ...
    save_fig(p, "bkt-per-capita")
    save_fig_data(p, "bkt-per-capita", names_from = "geo")

`plot` may also be a plain data frame, for a figure whose data to widen
isn't the `ggplot` object's own (say a summary table built alongside the
plot, or one with columns the plot itself never uses).

## See also

[`set_fig_data_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_data_defaults.md)
to change the defaults for every figure's data at once;
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
for the figure itself.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
  ggplot2::geom_point()
save_fig_data(p, "wt-mpg", dir = tempdir(), names_from = "cyl",
  values_from = "mpg")
} # }
```
