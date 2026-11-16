# Write a plot to `<dir>/<year>/<name>.<device>` for each device

Shared by
[`save_fig()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig.md)
and
[`save_fig_captioned()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_captioned.md):
validates `name`, creates the year folder and writes one file per format
in `opts$device`.

## Usage

``` r
.save_fig_files(plot, name, opts)
```

## Arguments

- plot:

  The already themed plot to write.

- name:

  File name without the extension.

- opts:

  A settings list with `dir`, `year`, `width`, `height`, `units`, `dpi`
  and `device`, as returned by
  [`fig_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_defaults.md)
  or
  [`fig_captioned_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_captioned_defaults.md).
