# Make Visit Labels Based on a Numeric Vector

Create a character vector of values to be used as labels for a
[factor](https://rdrr.io/r/base/factor.html).

## Usage

``` r
make_visit_labels(t, visit = "VIS", baseline = "BASELINE", pad = "0")
```

## Arguments

- t:

  A non-empty numeric vector of unique,
  [finite](https://rdrr.io/r/base/is.finite.html) elements in ascending
  order.

- visit:

  A single character string specifying the prefix to add to `t`.

- baseline:

  A single character string to use for the first timepoint's label.
  Alternatively, set to `NULL` so that all timepoints will have the
  prefix specified by `visit`.

- pad:

  The character to use to pad between `visit` and `t` so that the places
  of `t` are aligned. Alternatively, set to `NULL` so that `t` is
  automatically converted to `character` without special formatting.
  This can result in numbers in labels not being aligned or not being in
  "alphabetical" [order](https://rdrr.io/r/base/order.html).

## Value

A [character](https://rdrr.io/r/base/character.html) vector of length
[`length`](https://rdrr.io/r/base/length.html)`(t)`.

## Details

Places `visit` as a prefix before the values of `t`. If `pad` is not
`NULL`, the values of `t` are first `format`ted so that their places are
aligned, and they are left-padded with zeros.

If `baseline` is not `NULL` it is used as the first label regardless of
the value of `t[1]`.

Uses
[`make.unique`](https://rdrr.io/r/base/make.unique.html)`(sep = "_")` in
case any elements are identical after
[format](https://rdrr.io/r/base/format.html)ting.

## Examples

``` r
make_visit_labels(c(0, 5, 13, 101))
#> [1] "BASELINE" "VIS005"   "VIS013"   "VIS101"  

make_visit_labels(c(0, 5.23453, 13, 101.4))
#> [1] "BASELINE"     "VIS005.23453" "VIS013.00000" "VIS101.40000"

make_visit_labels(c(0, 5.23453, 13, 101.4), baseline = NULL, pad = " ")
#> [1] "VIS  0.00000" "VIS  5.23453" "VIS 13.00000" "VIS101.40000"

make_visit_labels(c(0, 5.23453, 13, 101.4), visit = "Week", pad = NULL)
#> [1] "BASELINE"    "Week5.23453" "Week13"      "Week101.4"  
```
