# Run a Natural Cubic Spline (NCS) Analysis.

Fit and analyze an
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
model wherein the continuous time variable has splines applied.

- `ncs_analysis()` fits such a model without involving subgroups.

- `ncs_analysis_subgroup()` fits a model that involves subgroups and
  performs additional analyses.

## Usage

``` r
ncs_analysis(
  data,
  response = "response",
  subject = "subject",
  arm = "arm",
  control_group,
  time_observed_continuous = "time_observed_continuous",
  df = 2,
  spline_basis = NULL,
  time_observed_index = "time_observed_index",
  time_scheduled_continuous = "time_scheduled_continuous",
  time_scheduled_baseline = 0,
  time_scheduled_label = "time_scheduled_label",
  covariates = ~1,
  cov_structs = c("us", "toeph", "ar1h", "csh", "cs"),
  cov_struct_group = NULL,
  mmrm_args = list(method = "Satterthwaite"),
  emmeans_args = list(nesting = NULL),
  average_nuisance = TRUE,
  conf.level = 0.95,
  change_in_bl_contrast_args = list(adjust = "none"),
  treatment_effect_contrast_args = list(adjust = "none"),
  confint_args = list(level = conf.level),
  return_models = FALSE,
  expand_spline_terms = TRUE
)

ncs_analysis_subgroup(
  data,
  response = "response",
  subject = "subject",
  arm = "arm",
  control_group,
  subgroup = "subgroup",
  subgroup_comparator = "subgroup1",
  time_observed_continuous = "time_observed_continuous",
  df = 2,
  spline_basis = NULL,
  time_observed_index = "time_observed_index",
  time_scheduled_continuous = "time_scheduled_continuous",
  time_scheduled_baseline = 0,
  time_scheduled_label = "time_scheduled_label",
  covariates = ~1,
  cov_structs = c("us", "toeph", "ar1h", "csh", "cs"),
  cov_struct_group = NULL,
  mmrm_args = list(method = "Satterthwaite"),
  emmeans_args = list(nesting = NULL),
  average_nuisance = TRUE,
  conf.level = 0.95,
  change_in_bl_contrast_args = list(adjust = "none"),
  treatment_effect_contrast_args = list(adjust = "none"),
  confint_args = list(level = conf.level),
  subgroup_interaction_test = TRUE,
  return_models = FALSE,
  expand_spline_terms = TRUE
)
```

## Arguments

- data:

  (`data frame`)  
  data set supplied to the `data` argument of
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
  when fitting models. The supplied expression is
  [quoted](https://rlang.r-lib.org/reference/enquo.html) and must
  evaluate to a data frame. See **Tidy evaluation support**.

- response:

  (`numeric` or `string`)  
  the response variable. It can be a `string` identifying the name of an
  existing variable; otherwise, the supplied expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) and added to
  the formula as is (see **Tidy evaluation support**).

- subject:

  (`atomic` or `string`)  
  the unique subject identifier forwarded to the `subject` argument of
  [`mmrm::cov_struct()`](https://openpharma.github.io/mmrm/latest-tag/reference/cov_struct.html).
  Ignored if `cov_structs` is a `list`. Can be a `string` identifying an
  existing variable; otherwise the supplied expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) and turned into
  a `string` with
  [`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
  (see **Tidy evaluation support**).

- arm:

  (`factor` or `string`)  
  the study arm. It must be a `string` or a
  [name](https://rdrr.io/r/base/name.html) identifying an existing
  variable (i.e., it cannot be a
  [call](https://rdrr.io/r/base/call.html)). If a name, it will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) before being
  added to the model formula (see **Tidy evaluation support**). If it
  does not evaluate to a [factor](https://rdrr.io/r/base/factor.html) or
  if `control_group` is not its first
  [level](https://rdrr.io/r/base/levels.html), the `data` argument will
  be wrapped in a
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  call that forces this to be the case.

- control_group:

  (`string`)  
  the value in `arm` denoting the control group. If necessary, `arm`
  will be preprocessed such that it is a factor with `control_group` as
  its first level.

- time_observed_continuous:

  (`numeric` or `string`)  
  the visit's *observed* time point. It must either be a `string` or a
  [name](https://rdrr.io/r/base/name.html) identifying an existing
  variable (i.e., it cannot be a
  [call](https://rdrr.io/r/base/call.html)). If a name is provided, it
  is [quoted](https://rlang.r-lib.org/reference/enquo.html) and
  incorporated into the model formula as is (see **Tidy evaluation
  support**).

- df:

  (scalar `integer`)  
  number of degrees of freedom to use to create the spline basis. Passed
  to the `df` argument of
  [`time_spline_basis()`](https://nikkrieger.github.io/splinetrials/reference/time_spline_basis.md).
  Ignored if the `spline_basis` argument is not `NULL`.

- spline_basis:

  (`basis` `matrix`)  
  a spline basis: probably a value returned by
  [`time_spline_basis()`](https://nikkrieger.github.io/splinetrials/reference/time_spline_basis.md)
  (which wraps [`splines::ns()`](https://rdrr.io/r/splines/ns.html)). If
  `NULL` (the default), then the spline basis will be the result of
  forwarding `time_observed_continuous` and `df` to
  [`time_spline_basis()`](https://nikkrieger.github.io/splinetrials/reference/time_spline_basis.md).
  See **Providing a spline basis**.

- time_observed_index:

  (`ordered` or `string`)  
  the visit index that the visit shall be associated with, based on the
  visit's *observed* time point. This will be passed as the `visits`
  argument of
  [`mmrm::cov_struct()`](https://openpharma.github.io/mmrm/latest-tag/reference/cov_struct.html).
  It can be a `string` identifying an existing variable; otherwise the
  supplied expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) and turned into
  a `string` with
  [`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
  (see **Tidy evaluation support**). If it does not evaluate to an
  `ordered` factor, it will be wrapped with
  [`as.ordered()`](https://rdrr.io/r/base/factor.html). Ignored if
  `cov_structs` is a `list`.

- time_scheduled_continuous:

  (`numeric` or `string`)  
  the continuous time point when the visit was *scheduled* to occur. Its
  unique values will identify the time points at which the marginal
  means and other results will be calculated. It can be a `string`
  identifying an existing variable name; otherwise the supplied
  expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) before being
  evaluated (see **Tidy evaluation support**).

- time_scheduled_baseline:

  (`scalar numeric`)  
  the continuous time point when baseline was *scheduled* to occur.
  Defaults to 0.

- time_scheduled_label:

  (`character` or `string`)  
  the label associated with the scheduled visit. It can be a `string`
  identifying an existing variable name; otherwise the supplied
  expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) before being
  evaluated (see **Tidy evaluation support**).

- covariates:

  (`formula`)  
  formula containing additional terms that should be added to the `mmrm`
  model. Defaults to `~ 1`, in which no additional terms will be added.
  Must not have a left side. Cannot contain `.`. To specify that the
  model shall not have an intercept, use include `+ 0` or `- 1` in this
  formula.

- cov_structs:

  (`character` or `list`)  
  either a `list` of unique
  [`cov_struct`](https://openpharma.github.io/mmrm/latest-tag/reference/cov_struct.html)
  objects or a `character` vector of one or more of the covariance
  structure abbreviations as described in
  [`mmrm::cov_types()`](https://openpharma.github.io/mmrm/latest-tag/reference/covariance_types.html).
  These covariance structures will be attempted in order until one of
  them achieves a converging model fit. Defaults to
  `c("us", "toeph", "ar1h", "csh", "cs")`.

- cov_struct_group:

  (`atomic` or `string`)  
  optional grouping variable to be passed to the `group` argument of
  [`mmrm::cov_struct()`](https://openpharma.github.io/mmrm/latest-tag/reference/cov_struct.html).
  It can be a `string` identifying an existing variable name; otherwise
  the supplied expression will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) and turned into
  a `string` with
  [`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
  (see **Tidy evaluation support**). Ignored if `cov_structs` is a
  `list`. Defaults to `NULL`, in which case no grouping variable will be
  used.

- mmrm_args:

  (named `list`)  
  arguments to be passed to
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).
  If any elements have the names `formula`, `data`, or `covariance` they
  will be ignored. An element named `vcov` will also be ignored unless
  fitting a model with an unstructured covariance. Defaults to
  `list(method = "Satterthwaite")`.

- emmeans_args:

  (named `list`)  
  arguments to be passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).
  If any elements have the names `object` `specs`, or `at` they will be
  ignored. If `average_nuisance = TRUE`, any element named `nuisance`
  will be ignored. Any elements named `params` may be ignored. Defaults
  to `list(nesting = NULL)`.

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

- conf.level:

  (scalar `numeric`)  
  confidence level for the calculation of p-values. Defaults to `0.95`.

- change_in_bl_contrast_args, treatment_effect_contrast_args:

  (named `list`)  
  arguments to be passed to
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
  when calculating the change from baseline and treatment effect
  results. If any elements have the names `object` or `method` they will
  be ignored. Defaults to `list(adjust = "none")`.

- confint_args:

  (named `list`)  
  arguments to be passed to
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html) when
  calculating confidence intervals for change in baseline and treatment
  effect. If any element has the name `object` it will be ignored.
  Defaults to `list(level = conf.level)`.

- return_models:

  (`flag`)  
  flag indicating whether or not to return the model(s) used to
  calculate the results. See **Obtaining the models used** below.

- expand_spline_terms:

  (`flag`)  
  flag indicating whether or not to separate the cubic spline matrix
  into separate terms (one for each degree of freedom). Defaults to
  `TRUE`. See **Expanding spline terms**.

- subgroup:

  (`factor` or `string`)  
  the subgroup. It must be a `string` or a
  [name](https://rdrr.io/r/base/name.html) identifying an existing
  variable (i.e., it cannot be a
  [call](https://rdrr.io/r/base/call.html)). If a name, it will be
  [quoted](https://rlang.r-lib.org/reference/enquo.html) before being
  added to the model formula (see **Tidy evaluation support**). If it
  does not evaluate to a [factor](https://rdrr.io/r/base/factor.html) or
  if `subgroup_comparator` is not its first
  [level](https://rdrr.io/r/base/levels.html), the `data` argument will
  be wrapped in a
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  call that forces this to be the case.

- subgroup_comparator:

  (`string`)  
  the value in `subgroup` denoting the "main" subgroup that all other
  subgroups should be compared to. If necessary, `subgroup` will be
  preprocessed such that it is a factor with `control_group` as its
  first level.

- subgroup_interaction_test:

  (`flag`)  
  flag indicating whether or not the subgroup interaction test should be
  performed. If `TRUE`, the returned value will include an `interaction`
  element, a data frame of results. Defaults to `TRUE`. See **Subgroup
  interaction test** for details.

## Value

For `ncs_analysis()`, see
[`splinetrials_analysis`](https://nikkrieger.github.io/splinetrials/reference/splinetrials_analysis-class.md).
For `ncs_analysis_subgroup()`, see
[`splinetrials_subgroup_analysis`](https://nikkrieger.github.io/splinetrials/reference/splinetrials_subgroup_analysis-class.md).

## Overview

These functions create an
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
model from the user-specified arguments. They then perform a series of
analyses and produce a data frame of results with a unique row for each
combination of `arm`, `time_scheduled_continuous`, and `subgroup` (for
`ncs_analysis_subgroup()` only). The results include:

1.  Basic diagnostics on the response variable

2.  Estimated marginal means

3.  Change from baseline

4.  Treatment effect

5.  Percent slowing

## Building a model

See the details of
[`ncs_mmrm_fit()`](https://nikkrieger.github.io/splinetrials/reference/ncs_mmrm_fit.md)
for information on how the model is built.

## Subgroup analysis

`ncs_analysis_subgroup()` contains more analyses and results than
`ncs_analysis()`. Whereas the latter produces a data frame by default,
the former produces a list of data frames.

### Treatment effects

The treatment effect is calculated twice: once *between* subgroups
(examining the differences between the subgroups within each study arm)
and once *within* subgroups (examining the differences between the study
arms within each subgroup). The main results table is effectively
returned twice as both the `between` element and the `within` element.
These elements' treatment effect values differ, and only the `within`
element contains the percent slowing analysis results.

### Type-III ANOVA

The subgroup analyses include a type-III analysis of variance (ANOVA) on
the main analysis model's terms, using a Chi-squared test statistic.
This is accomplished via the `mmrm` method for
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). The results
are included in the returned value as the `type3` element. See
[`vignette("hypothesis_testing", "mmrm")`](https://openpharma.github.io/mmrm/latest-tag/articles/hypothesis_testing.html)
for details on the type-III ANOVA.

### Subgroup interaction test

When `subgroup_interaction_test = TRUE`, the function runs an ANOVA to
compare a maximum-likelihood-estimated (ML) version of the original
model to a reduced version. This happens as follows:

1.  The original analysis model is refit with `reml = FALSE` if it was
    originally created with `reml = TRUE`. This may be dubbed the "full"
    model.

2.  A reduced version of the "full" model is created, removing the
    second-order interaction term (see the **`arm` and `subgroup`
    terms** section above). This may be dubbed the "reduced" model.

3.  The "full" and "reduced" models are compared using the `mmrm` method
    of [`stats::anova()`](https://rdrr.io/r/stats/anova.html).

4.  The results are processed into a table and added to the returned
    value as the `interaction` element.

## Returning the models used

The model(s) used to conduct the analyses can be obtained by setting
`return_models = TRUE`.

For `ncs_analysis()`, the analysis model will be included as the
`splinetrials_analysis_model`
[attr](https://rdrr.io/r/base/attr.html)ibute of the returned value.

For `ncs_analysis_subgroup()`, the analysis model is added to the
returned value as the `analysis_model` element. Furthermore, if
`subgroup_interaction_test = TRUE`, the "full" and "reduced" models will
be included in the returned value as the elements `full` and `reduced`
(see **Subgroup interaction test** above for details).

## Examples

``` r
if (FALSE) { # interactive()
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

# Without subgroup:
ncs_analysis(
  data = fev_mod,
  response = FEV1,
  subject = USUBJID,
  arm = ARMCD,
  control_group = "PBO",
  time_observed_continuous = time_cont,
  df = 2,
  time_observed_index = obs_visit_index,
  time_scheduled_continuous = VISITN,
  time_scheduled_baseline = 10,
  time_scheduled_label = AVISIT,
  covariates = ~ FEV1_BL + RACE,
  cov_structs = c("ar1", "us")
)

# With subgroup:
ncs_analysis_subgroup(
  data = fev_mod,
  response = FEV1,
  subject = USUBJID,
  arm = ARMCD,
  control_group = "PBO",
  subgroup = SEX,
  subgroup_comparator = "Male",
  time_observed_continuous = time_cont,
  df = 2,
  time_observed_index = obs_visit_index,
  time_scheduled_continuous = VISITN,
  time_scheduled_baseline = 10,
  time_scheduled_label = AVISIT,
  covariates = ~ FEV1_BL + RACE,
  cov_structs = c("ar1", "us")
)
}
```
