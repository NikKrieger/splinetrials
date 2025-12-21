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
if (FALSE) { # interactive()
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
    arm = ARMCD,
    control_group = "PBO",
    subgroup = SEX,
    subgroup_comparator = "Male",
    covariates = ~ FEV1_BL + RACE
  )

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

# Same thing as a tibble:
change_from_baseline(
  emmeans = marginal_means,
  time_observed_continuous = "time_cont",
  time_scheduled_baseline = 10,
  arm = "ARMCD",
  subgroup = "SEX",
  as_tibble = TRUE
)

treatment_effect(
  emmeans = marginal_means,
  time_observed_continuous = "time_cont",
  time_scheduled_baseline = 10,
  arm = "ARMCD",
  subgroup = "SEX",
  ref_value = "Male",
  as_tibble = TRUE
)
}
```
