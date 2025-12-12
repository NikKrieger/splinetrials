# Categorize Observed Timepoints According to Scheduled Timepoints

Create an [ordered](https://rdrr.io/r/base/factor.html) factor from a
vector of observed values, associating each observed value with the
[level](https://rdrr.io/r/base/levels.html) corresponding to a vector of
expected/`scheduled` values.

## Usage

``` r
bin_timepoints(
  observed,
  scheduled = unique(observed[!is.na(observed)]),
  breaks = c(-Inf, midpoints(scheduled), Inf),
  labels = make_visit_labels(seq_along(scheduled) - 1),
  ...
)
```

## Arguments

- observed:

  A numeric vector of values.

- scheduled:

  A numeric vector of unique,
  [finite](https://rdrr.io/r/base/is.finite.html) values. Length must be
  at least 2. The default is to take the
  [unique](https://rdrr.io/r/base/unique.html),
  [finite](https://rdrr.io/r/base/is.finite.html) values of `observed`.

- breaks:

  A numeric vector of unique values. `-Inf` and `Inf` are valid. Passed
  to [`cut()`](https://rdrr.io/r/base/cut.html). The default is to take
  the midpoints of `scheduled` and to put them in between
  `c(-Inf, [Inf])`.

- labels:

  A vector of labels for the resulting
  [`ordered`](https://rdrr.io/r/base/factor.html) factor. Passed to
  [`cut()`](https://rdrr.io/r/base/cut.html). Must have
  [`length()`](https://rdrr.io/r/base/length.html) equal to `scheduled`.
  Defaults to `"Baseline"` as the first level's label and `"Visit#"` for
  all subsequent [levels](https://rdrr.io/r/base/levels.html), where `#`
  is the numeric index of the timepoint minus 1.

- ...:

  Additional arguments passed to
  [`cut()`](https://rdrr.io/r/base/cut.html).

## Value

And \[`ordered] factor` with the same length as `observed`.

## Examples

``` r
observed_timepoints <- c(0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
scheduled_timepoints <- c(0, 1, 2, 3, 4, 5, 10, 15, 20, 30, 50, 75)
bin_timepoints(
  observed_timepoints,
  scheduled = scheduled_timepoints
)
#>  [1] BASELINE VIS01    VIS02    VIS03    VIS05    VIS06    VIS07    VIS08   
#>  [9] VIS09    VIS10    VIS11   
#> 12 Levels: BASELINE < VIS01 < VIS02 < VIS03 < VIS04 < VIS05 < ... < VIS11

bin_timepoints(
  observed_timepoints,
  scheduled = scheduled_timepoints,
  breaks = c(-Inf, 0.1, 1.5, 2.5, 3.5, 4.4, 7, 11, 15.1, 21, 31, 58, 80)
)
#>  [1] BASELINE VIS01    VIS02    VIS03    VIS05    VIS06    VIS07    VIS08   
#>  [9] VIS10    VIS10    <NA>    
#> 12 Levels: BASELINE < VIS01 < VIS02 < VIS03 < VIS04 < VIS05 < ... < VIS11

bin_timepoints(
  observed_timepoints,
  scheduled = scheduled_timepoints,
  labels = month.name
)
#>  [1] January   February  March     April     June      July      August   
#>  [8] September October   November  December 
#> 12 Levels: January < February < March < April < May < June < ... < December

bin_timepoints(
  observed_timepoints,
  scheduled = scheduled_timepoints,
  labels = make_visit_labels(scheduled_timepoints, visit = "Week")
)
#>  [1] BASELINE Week01   Week02   Week03   Week05   Week10   Week15   Week20  
#>  [9] Week30   Week50   Week75  
#> 12 Levels: BASELINE < Week01 < Week02 < Week03 < Week04 < Week05 < ... < Week75

bin_timepoints(observed_timepoints)
#>  [1] BASELINE VIS01    VIS02    VIS03    VIS04    VIS05    VIS06    VIS07   
#>  [9] VIS08    VIS09    VIS10   
#> 11 Levels: BASELINE < VIS01 < VIS02 < VIS03 < VIS04 < VIS05 < ... < VIS10
```
