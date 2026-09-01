# Terms of trade adjusted volume of GDP

Real GDP measures what a country produces, not what it can buy with it.
When export prices rise relative to import prices the same production
buys more imports, and that trading gain does not show up in the volume
of GDP.

## Usage

``` r
gdp_trading_gain(gdp, exports, exports_cp, imports, imports_cp)
```

## Arguments

- gdp, exports, imports:

  Chain linked volumes.

- exports_cp, imports_cp:

  The same exports and imports at current prices.

## Value

A numeric vector in the units of `gdp`.

## Details

The adjusted measure, also called command basis GDP, replaces the volume
of exports with the volume of imports those exports could pay for:

\$\$gdp^{adj} = gdp - exports + \frac{exports\_{cp}}{p^{imports}}\$\$

where the import deflator is \\p^{imports} = imports\_{cp} / imports\\.
Used as the output of
[`ind_ulc()`](https://tuottavuuslautakunta.github.io/fiprod/reference/ind_ulc.md)
it gives the terms of trade adjusted unit labour cost, which is the
measure that says whether a country's cost level is sustainable given
the prices it actually gets for its exports.

## See also

[`ind_ulc()`](https://tuottavuuslautakunta.github.io/fiprod/reference/ind_ulc.md)

## Examples

``` r
# Export prices up 10 % against import prices: the adjusted volume is higher
gdp_trading_gain(gdp = 100, exports = 40, exports_cp = 44,
                 imports = 30, imports_cp = 30)
#> [1] 104
```
