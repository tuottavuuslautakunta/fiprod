# Where package data is written during development

Resolved from the package root rather than from the working directory,
so that a call from `vignettes/` does not create
`vignettes/inst/extdata`.

## Usage

``` r
.pkg_extdata_dir()
```

## Value

A path to `inst/extdata`.
