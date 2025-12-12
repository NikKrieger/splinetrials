# Calculate the Change from Baseline or Treatment Effects from Estimated Marginal Means

Pass
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
objects (probably obtained via
[`ncs_emmeans()`](https://nikkrieger.github.io/splinetrials/reference/ncs_emmeans.md))
to
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
using specially constructed contrast matrices so that change from
baseline and treatment effects can be calculated.

- `change_from_baseline` calculate the change from baseline for each of
  the different study arms/subgroups.

- `treatment_effect()` calculate the treatment effect for each study arm
  when there is no subgroup. When there is a subgroup, calculate the
  treatment effect *between* subgroups (examining the differences
  *between* the subgroups within each study arm) or *within* subgroups
  (examining the differences between the study arms *within* each
  subgroup).

## Usage

``` r
change_from_baseline(
  emmeans,
  time_observed_continuous = emmeans@roles$predictors[2],
  time_scheduled_baseline = 0,
  arm = emmeans@roles$predictors[1],
  subgroup = if (length(emmeans@roles$predictors) == 3) emmeans@roles$predictors[3],
  contrast_args = list(adjust = "none"),
  ...,
  as_tibble = FALSE,
  confint_args = list(level = 0.95)
)

treatment_effect(
  emmeans,
  time_observed_continuous = emmeans@roles$predictors[2],
  time_scheduled_baseline,
  arm = emmeans@roles$predictors[1],
  subgroup = if (length(emmeans@roles$predictors) == 3) emmeans@roles$predictors[3],
  ref_value,
  subgroup_type = c("between", "within"),
  contrast_args = list(adjust = "none"),
  ...,
  as_tibble = FALSE,
  confint_args = list(level = 0.95)
)
```

## Arguments

- emmeans:

  (`emmGrid`)  
  an object of class
  [`emmGrid`](https://rvlenth.github.io/emmeans/reference/emmGrid-class.html),
  ideally obtained via
  [`ncs_emmeans()`](https://nikkrieger.github.io/splinetrials/reference/ncs_emmeans.md),
  which wraps
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- time_scheduled_baseline:

  (`scalar numeric`)  
  the continuous time point when baseline was *scheduled* to occur.
  Defaults to 0.

- arm, time_observed_continuous, subgroup:

  (`string`)  
  strings identifying the study arm variable, *observed* continuous time
  variable, and (optionally) subgroup variable supplied to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html),
  probably via
  [`ncs_emmeans()`](https://nikkrieger.github.io/splinetrials/reference/ncs_emmeans.md)).
  If
  [`ncs_emmeans()`](https://nikkrieger.github.io/splinetrials/reference/ncs_emmeans.md)
  was indeed used, these strings *should* be contained in the character
  vector `emmeans@roles$predictors` (see the default arguments).

- contrast_args, ...:

  (named `list`)  
  arguments to be passed to
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).
  Any arguments with the names `object` or `method` will be overwritten.
  Arguments in `contrast_args` override identically named arguments in
  `...`.

- as_tibble:

  (`flag`)  
  `TRUE` or `FALSE` indicating whether or not the results of
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  should be processed and returned as a
  [tibble](https://dplyr.tidyverse.org/reference/reexports.html).

- confint_args:

  (named `list`)  
  arguments to be passed to
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html) when
  calculating confidence intervals. Ignored if `as_tibble = FALSE`. If
  `NULL`, confidence intervals will not be calculated. Defaults to
  `list(level = 0.95)`.

- ref_value:

  (`string`)  
  the value in `arm` (if `subgroup = NULL` *or* if
  `subgroup_type = "within"`) or the value in `subgroup` (if `subgroup`
  is not `NULL` *and* `subgroup_type = "between"`) denoting the control
  group.

- subgroup_type:

  (`string`)  
  either `"between"` or `"within"`, denoting whether to calculate the
  treatment effect *between* subgroups (examining the differences
  between the subgroups within each study arm) and once *within*
  subgroups (examining the differences between the study arms within
  each subgroup).

## Value

When `as_tibble = FALSE`, the value returned by
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).
If `as_tibble = TRUE`, a
[tibble](https://dplyr.tidyverse.org/reference/reexports.html):

1.  {*column name will be the value of the `arm` argument*}: the study
    arm.

2.  {*column name will be the value of the `time_observed_continuous`
    argument*}: the *observed* continuous time variable.

3.  {*column name will be the value of the `subgroup` argument*}: the
    subgroup. **Only present if `subgroup` is not `NULL`.**

4.  `estimate`: estimate for change from baseline or treatment effect.

5.  `SE`: standard error of `estimate`.

6.  `df`: degrees of freedom for calculating the confidence interval for
    and estimating the significance of `estimate`.

7.  `lower.CL`: lower bound of confidence interval for `estimate`.
    **Only present if `confint_args` is not `NULL`.**

8.  `upper.CL`: upper bound of confidence interval for `estimate`.
    **Only present if `confint_args` is not `NULL`.**

9.  `t.ratio`: test statistic measuring the significance of `estimate`.

10. `p.value`: p-value for the significance of `estimate`.

## Examples

``` r
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

fit <-
  ncs_mmrm_fit(
    data = fev_mod,
    type = "subgroup_full",
    response = FEV1,
    subject = USUBJID,
    cov_structs = c("ar1", "us"),
    time_observed_continuous = time_cont,
    df = 2,
    time_observed_index = obs_visit_index,
    time_scheduled_continuous = VISITN,
    arm = ARMCD,
    control_group = "PBO",
    subgroup = SEX,
    subgroup_comparator = "Male",
    covariates = ~ FEV1_BL + RACE
  )
#> In as.ordered(obs_visit_index) there are dropped visits: 27, 37, 43.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.

marginal_means <-
  ncs_emmeans(
    fit = fit,
    observed_time = "time_cont",
    scheduled_time = "VISITN",
    arm = "ARMCD",
    subgroup = "SEX"
  )

change_from_baseline(
  emmeans = marginal_means,
  time_observed_continuous = "time_cont",
  time_scheduled_baseline = 10,
  arm = "ARMCD",
  subgroup = "SEX"
)
#>  contrast      estimate    SE   df t.ratio p.value
#>  PBO_20_Male       4.75 0.712 75.7   6.663  <.0001
#>  TRT_20_Male       6.98 0.759 68.8   9.207  <.0001
#>  PBO_30_Male       9.84 0.920 73.1  10.700  <.0001
#>  TRT_30_Male      12.40 1.030 69.3  11.998  <.0001
#>  PBO_40_Male      15.25 1.870 45.6   8.147  <.0001
#>  TRT_40_Male      16.44 1.610 40.5  10.239  <.0001
#>  PBO_20_Female     3.29 0.749 81.2   4.396  <.0001
#>  TRT_20_Female     4.45 0.680 82.4   6.550  <.0001
#>  PBO_30_Female     8.27 0.940 79.3   8.798  <.0001
#>  TRT_30_Female     9.64 0.977 79.2   9.867  <.0001
#>  PBO_40_Female    14.75 1.510 46.4   9.779  <.0001
#>  TRT_40_Female    15.46 1.960 48.3   7.897  <.0001
#> 
#> Results are averaged over the levels of: 2 nuisance factors, obs_visit_index 

# Same thing as a tibble:
change_from_baseline(
  emmeans = marginal_means,
  time_observed_continuous = "time_cont",
  time_scheduled_baseline = 10,
  arm = "ARMCD",
  subgroup = "SEX",
  as_tibble = TRUE
)
#> # A tibble: 12 × 10
#>    ARMCD time_cont SEX   estimate    SE    df lower.CL upper.CL t.ratio  p.value
#>    <fct>     <dbl> <fct>    <dbl> <dbl> <dbl>    <dbl>    <dbl>   <dbl>    <dbl>
#>  1 PBO          20 Male      4.75 0.712  75.7     3.33     6.17    6.66 3.83e- 9
#>  2 TRT          20 Male      6.98 0.759  68.8     5.47     8.50    9.21 1.30e-13
#>  3 PBO          30 Male      9.84 0.920  73.1     8.01    11.7    10.7  1.30e-16
#>  4 TRT          30 Male     12.4  1.03   69.3    10.3     14.5    12.0  1.40e-18
#>  5 PBO          40 Male     15.2  1.87   45.6    11.5     19.0     8.15 1.90e-10
#>  6 TRT          40 Male     16.4  1.61   40.5    13.2     19.7    10.2  8.51e-13
#>  7 PBO          20 Fema…     3.29 0.749  81.2     1.80     4.78    4.40 3.31e- 5
#>  8 TRT          20 Fema…     4.45 0.680  82.4     3.10     5.80    6.55 4.62e- 9
#>  9 PBO          30 Fema…     8.27 0.940  79.3     6.40    10.1     8.80 2.33e-13
#> 10 TRT          30 Fema…     9.64 0.977  79.2     7.69    11.6     9.87 1.95e-15
#> 11 PBO          40 Fema…    14.7  1.51   46.4    11.7     17.8     9.78 7.49e-13
#> 12 TRT          40 Fema…    15.5  1.96   48.3    11.5     19.4     7.90 3.02e-10

treatment_effect(
  emmeans = marginal_means,
  time_observed_continuous = "time_cont",
  time_scheduled_baseline = 10,
  arm = "ARMCD",
  subgroup = "SEX",
  ref_value = "Male",
  as_tibble = TRUE
)
#> # A tibble: 6 × 10
#>   ARMCD time_cont SEX    estimate    SE    df lower.CL upper.CL t.ratio p.value
#>   <fct>     <dbl> <fct>     <dbl> <dbl> <dbl>    <dbl>    <dbl>   <dbl>   <dbl>
#> 1 PBO          20 Female   -1.46   1.03 155.     -3.50   0.587   -1.41   0.161 
#> 2 TRT          20 Female   -2.53   1.02 144.     -4.55  -0.515   -2.48   0.0142
#> 3 PBO          30 Female   -1.57   1.31 150.     -4.17   1.03    -1.19   0.234 
#> 4 TRT          30 Female   -2.77   1.41 144.     -5.56   0.0259  -1.96   0.0521
#> 5 PBO          40 Female   -0.499  2.40  91.8    -5.27   4.27    -0.208  0.836 
#> 6 TRT          40 Female   -0.977  2.53  86.6    -6.00   4.04    -0.387  0.700 
```
