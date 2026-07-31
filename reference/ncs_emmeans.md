# Estimate Marginal Means for a Natural Cubic Splines Analysis

This is wrapper around
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
for a natural cubic splines analysis in which there is a continuous time
variable, a study arm, and (optionally) a subgroup variable.

## Usage

``` r
ncs_emmeans(
  fit,
  data = fit[["data"]],
  observed_time = NULL,
  scheduled_time = NULL,
  arm = NULL,
  subgroup = NULL,
  average_nuisance = TRUE,
  emmeans_args = list(nesting = NULL),
  ...,
  scheduled_time_spec = sort(unique(data[[scheduled_time]])),
  arm_spec = as.character(sort(unique(data[[arm]]))),
  subgroup_spec = as.character(sort(unique(data[[subgroup]]))),
  .__caller_env = rlang::caller_env()
)
```

## Arguments

- fit:

  (`mmrm`)  
  an `mmrm` object whose terms include the variables supplied to
  `observed_time`, `scheduled_time`, `arm`, and (optionally) `subgroup`.

- data:

  (`data frame`)  
  a data frame on which to estimate marginal means. Defaults to
  `fit[["data"]]`.

- observed_time:

  (`string`)  
  string specifying the *observed* continuous time variable in both
  `fit` and in `data`.

- scheduled_time:

  (`string`)  
  string specifying the *scheduled* continuous time variable in both
  `fit` and in `data`. Ignored if `scheduled_time_spec` is provided.

- arm:

  (`string`)  
  string specifying the study arm variable in both `fit` and in `data`.

- subgroup:

  (`string`)  
  string specifying the subgroup variable in both `fit` and in `data`.

- average_nuisance:

  (`flag`)  
  flag indicating whether the names of the terms in `covariates` should
  be supplied as the `nuisance` argument to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).
  This results in treating all the covariates as nuisance parameters and
  averaging over them when calculating the reference grid to estimate
  marginal means. See
  [`emmeans::ref_grid()`](https://rvlenth.github.io/emmeans/reference/ref_grid.html)
  for details and limitations.

- emmeans_args, ...:

  (named `list`)  
  arguments to be passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).
  If any elements have the names `object`, `specs`, or `at` they will be
  ignored. If `average_nuisance = TRUE`, any element named `nuisance`
  will be ignored. Any elements named `params` may be ignored.
  `emmeans_args` defaults to `list(nesting = NULL)`. Arguments named in
  `emmeans_args` supersede any named arguments in `...`.

- scheduled_time_spec:

  (`numeric`)  
  vector of unique, non-missing time points on which to calculate
  marginal means. Defaults to `sort(unique(data[[scheduled_time]]))`.

- arm_spec:

  (`character`)  
  vector of unique study arm values on which to calculate marginal
  means. Defaults to `as.character(sort(unique(data[[arm]])))`.

- subgroup_spec:

  vector of unique subgroup values on which to calculate marginal means.
  Ignored if `subgroup` is `NULL`. Defaults to
  `as.character(sort(unique(data[[subgroup]])))`.

- .\_\_caller_env:

  (`environment`)  
  the environment from which this function was called. Defaults to
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).

## Value

An object of class
[`emmGrid`](https://rvlenth.github.io/emmeans/reference/emmGrid-class.html):
the result of
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).
Note that for a result `result`, the elements
`result@model.info$nesting` and `result@misc$display` are removed.

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
    arm = ARMCD,
    control_group = "PBO",
    subgroup = SEX,
    subgroup_comparator = "Male",
    covariates = ~ FEV1_BL + RACE
  )
#> In as.ordered(obs_visit_index) there are dropped visits: 7.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.

ncs_emmeans(
  fit = fit,
  observed_time = "time_cont",
  scheduled_time = "VISITN",
  arm = "ARMCD",
  subgroup = "SEX"
)
#>  ARMCD time_cont SEX    emmean    SE   df lower.CL upper.CL
#>  PBO          10 Male     32.7 0.831 59.2     31.0     34.3
#>  TRT          10 Male     35.6 0.828 58.4     34.0     37.3
#>  PBO          20 Male     38.0 0.669 42.6     36.6     39.3
#>  TRT          20 Male     43.1 0.759 35.8     41.6     44.6
#>  PBO          30 Male     42.9 0.509 47.5     41.8     43.9
#>  TRT          30 Male     48.5 0.832 39.5     46.9     50.2
#>  PBO          40 Male     47.3 1.670 33.9     43.9     50.7
#>  TRT          40 Male     52.2 1.620 30.9     48.8     55.5
#>  PBO          10 Female   35.4 0.757 68.4     33.9     36.9
#>  TRT          10 Female   36.8 0.856 69.6     35.1     38.5
#>  PBO          20 Female   38.6 0.602 48.1     37.4     39.8
#>  TRT          20 Female   41.2 0.696 45.9     39.8     42.6
#>  PBO          30 Female   43.3 0.619 54.3     42.1     44.6
#>  TRT          30 Female   46.4 0.654 51.3     45.1     47.7
#>  PBO          40 Female   49.6 1.280 36.8     47.0     52.2
#>  TRT          40 Female   52.5 1.720 36.4     49.0     56.0
#> 
#> Results are averaged over the levels of: 2 nuisance factors, obs_visit_index 
#> Confidence level used: 0.95 
```
