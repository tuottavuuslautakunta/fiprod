# Write a data frame to `<dir>/<year>/<name>.xlsx`

Shared internal for
[`save_fig_data()`](https://tuottavuuslautakunta.github.io/fiprod/reference/save_fig_data.md):
validates `name`, creates the year folder and writes `data` as a
single-sheet xlsx file.

## Usage

``` r
.save_fig_data_file(data, name, opts)
```

## Arguments

- data:

  The (already widened, if wanted) data frame to write.

- name:

  File name without the extension.

- opts:

  A settings list with `dir` and `year`, as returned by
  [`fig_data_defaults()`](https://tuottavuuslautakunta.github.io/fiprod/reference/set_fig_data_defaults.md).
