# Midpoints of a Numeric Vector

Returns the midpoints between the elements of a vector in the order the
elements appear.

## Usage

``` r
midpoints(x)
```

## Arguments

- x:

  A numeric vector with at least 2 elements.

## Value

A numeric vector of [length](https://rdrr.io/r/base/length.html)
`length(x) - 1`.

## Details

This function does not sort.

## Examples

``` r
midpoints(c(0, 1, 10, 4))
#> [1] 0.5 5.5 7.0
```
