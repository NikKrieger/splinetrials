# Plot Outcome Variable by Timepoint and Study Arm

[Plot](https://ggplot2.tidyverse.org/reference/ggplot2-package.html) a
continuous outcome for each combination of scheduled visit and study
arm.

## Usage

``` r
plot_outcome_by_visit_and_group(
  data,
  outcome_var,
  scheduled_timepoint_var,
  group_var,
  ...,
  geom = ggplot2::geom_boxplot,
  geom_args = list(na.rm = TRUE)
)
```

## Arguments

- data:

  (`data frame`)  
  The data frame that will be supplied to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- outcome_var:

  (`numeric`)  
  The continuous outcome variable to supply to the `y` argument of
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Whatever is supplied will be [quoted and
  evaluated](https://rlang.r-lib.org/reference/embrace-operator.html)
  [in the context
  of](https://rlang.r-lib.org/reference/topic-data-mask.html) `data`.

- scheduled_timepoint_var:

  ([`ordered`](https://rdrr.io/r/base/factor.html))  
  The variable containing the scheduled timepoints to supply to the `x`
  argument of
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)..
  Whatever is supplied will be [quoted and
  evaluated](https://rlang.r-lib.org/reference/embrace-operator.html)
  [in the context
  of](https://rlang.r-lib.org/reference/topic-data-mask.html) `data`.

- group_var:

  (`numeric`)  
  The grouping variable (probably the study arm) to supply to the `fill`
  argument of
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)..
  Whatever is supplied will be [quoted and
  evaluated](https://rlang.r-lib.org/reference/embrace-operator.html)
  [in the context
  of](https://rlang.r-lib.org/reference/topic-data-mask.html) `data`.

- ...:

  Forwarded onto
  [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`ggplot2::aes`](https://ggplot2.tidyverse.org/reference/aes.html)`)`.

- geom:

  (`function`)  
  The `ggplot2` "geom" to use. Defaults to
  [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).

- geom_args:

  (`list`)  
  A list of arguments to supply to `geom`. Defaults to
  `list(na.rm = TRUE)`.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

plot_outcome_by_visit_and_group(
    data = fev_mod,
    outcome_var = FEV1,
    scheduled_timepoint_var = as.ordered(VISITN),
    group_var = ARMCD
)
```
