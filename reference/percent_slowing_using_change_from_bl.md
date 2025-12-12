# Calculates Percent Slowing from a Data Frame of Change-from-Baseline Data

Accepts a data frame of change-from-baseline data (probably created with
[`change_from_baseline()`](https://nikkrieger.github.io/splinetrials/reference/treatment_effect.md))
and returns a table of percent slowing results.

## Usage

``` r
percent_slowing_using_change_from_bl(
  change_from_bl_tbl,
  time_observed_continuous,
  arm,
  control_group,
  subgroup = NULL,
  est = "estimate",
  se = "SE",
  conf.level = 0.95
)
```

## Arguments

- change_from_bl_tbl:

  (data frame)  
  a data frame of change-from-baseline data whose columns include
  `time_observed_continuous`, `arm`, (optionally) `subgroup`, `est`, and
  `se`.

- time_observed_continuous, arm, subgroup, est, se:

  (`string`)  
  strings identifying the columns in `change_from_bl_tbl` that contain
  the continuous time variable, the study arm, (optionally) the
  subgroup, the change-from-baseline estimate, and the
  change-from-baseline standard error.

- control_group:

  (`string`)  
  the value in the `arm` column of `change_from_bl_tbl` denoting the
  control group.

- conf.level:

  (scalar `numeric`)  
  confidence level for the calculation of p-values. Defaults to `0.95`.

## Value

A data frame with a row for each combination of the unique values of
`change_from_bl_tbl[[time_observed_continuous]]`,
`change_from_bl_tbl[[arm]]` (except the value denoted in
`control_group`), and `change_from_bl_tbl[[subgroup]]` (if `subgroup` is
not `NULL`). It will contain the following columns:

1.  {*column name will be the value of the *`arm`* argument*}: the study
    arm.

2.  {*column name will be the value of the *`time_observed_continuous`*
    argument*}: the *observed* continuous time variable.

3.  {*column name will be the value of the *`subgroup`* argument*}: the
    subgroup. Only present if `subgroup` is not `NULL`.

4.  `percent_slowing_est`: the percent slowing estimate

5.  `percent_slowing_lower`: the lower bound of the confidence interval
    for `percent_slowing_est`.

6.  `percent_slowing_lower`: the upper bound of the confidence interval
    for `percent_slowing_est`.

## Details

For each study arm that is not the control group,

\$\$ \text{Let } \theta = \frac{\text{treatment estimate}}{\text{control
estimate}} \$\$\$\$ \text{Let } \alpha = 1 - \code{conf.level} \$\$\$\$
\text{Let MOE} = 100 \times z\_{1 - \alpha/2} \times
\frac{\sqrt{\text{treatment SE}^2 + (\theta \times \text{control
SE})^2}}{\|\text{control estimate}\|} \$\$

Therefore, the percent slowing estimates and their respective confidence
intervals are calculated thus:

\$\$ \text{Percent slowing estimate} = (1 - \theta) \times 100 \$\$\$\$
\text{Percent slowing CI} = \text{Percent slowing estimate} \pm
\text{MOE} \$\$

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
#> In as.ordered(obs_visit_index) there are dropped visits: 43.
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

change_from_bl_tbl <-
  change_from_baseline(
    emmeans = marginal_means,
    time_observed_continuous = "time_cont",
    time_scheduled_baseline = 10,
    arm = "ARMCD",
    subgroup = "SEX",
    as_tibble = TRUE
  )

percent_slowing_using_change_from_bl(
  change_from_bl_tbl = change_from_bl_tbl,
  time_observed_continuous = "time_cont",
  arm = "ARMCD",
  control_group = "PBO",
  subgroup = "SEX"
)
#> # A tibble: 6 × 6
#>   ARMCD time_cont SEX    percent_slowing_est percent_slowing_lower
#>   <fct>     <dbl> <fct>                <dbl>                 <dbl>
#> 1 TRT          20 Male                -48.2                 -106. 
#> 2 TRT          30 Male                -28.7                  -61.9
#> 3 TRT          40 Male                -11.7                  -47.9
#> 4 TRT          20 Female              -30.4                  -98.8
#> 5 TRT          30 Female              -16.3                  -50.7
#> 6 TRT          40 Female               -7.13                 -40.1
#> # ℹ 1 more variable: percent_slowing_upper <dbl>
```
