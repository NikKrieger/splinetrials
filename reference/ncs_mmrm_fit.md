# Create a Mixed Model with Repeated Measures Using Natural Cubic Splines.

Builds an
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
model that includes a study arm, optionally a subgroup, and natural
cubic splines applied to a continuous time variable. A wrapper around
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

Constructs a call to
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
for ncs analysis. Implements natural cubic splines for the continuous
time variable. Attempts a sequence of covariance structures in order
until one of them successfully converges. Title

## Usage

``` r
ncs_mmrm_fit(
  data,
  type = c("basic", "subgroup_full", "subgroup_reduced"),
  response,
  subject,
  cov_structs = c("us", "toeph", "ar1h", "csh", "cs"),
  cov_struct_group = NULL,
  time_observed_continuous,
  df = 2,
  spline_basis = NULL,
  time_observed_index,
  arm = NULL,
  control_group = "control",
  subgroup = NULL,
  subgroup_comparator = NULL,
  covariates = ~1,
  expand_spline_terms = TRUE,
  mmrm_args = list(method = "Satterthwaite"),
  ...
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

- type:

  (`string`)  
  one of `"basic"`, `"subgroup_full"`, or `"subgroup_reduced"`.

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

- covariates:

  (`formula`)  
  formula containing additional terms that should be added to the `mmrm`
  model. Defaults to `~ 1`, in which no additional terms will be added.
  Must not have a left side. Cannot contain `.`. To specify that the
  model shall not have an intercept, use include `+ 0` or `- 1` in this
  formula.

- expand_spline_terms:

  (`flag`)  
  flag indicating whether or not to separate the cubic spline matrix
  into separate terms (one for each degree of freedom). Defaults to
  `TRUE`. See **Expanding spline terms**.

- mmrm_args:

  (named `list`)  
  arguments to be passed to
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).
  If any elements have the names `formula`, `data`, or `covariance` they
  will be ignored. An element named `vcov` will also be ignored unless
  fitting a model with an unstructured covariance. Defaults to
  `list(method = "Satterthwaite")`.

- ...:

  additional arguments to be passed to
  [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).
  If any elements have the names `formula`, `data`, or `covariance` they
  will be ignored. An element named `vcov` will also be ignored unless
  fitting a model with an unstructured covariance. Defaults to
  `list(method = "Satterthwaite")`. Arguments named in `mmrm_args`
  supersede any named arguments in `...`.

## Value

An `mmrm` object created by
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

## Providing a spline basis

This function's `spline_basis` argument was designed with
[`splines::ns()`](https://rdrr.io/r/splines/ns.html) in mind, which
creates a matrix object with classes `basis` and `matrix` as well as
multiple attributes. In theory, `spline_basis` does not have to be a
`matrix`; however, it still must have a
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) method
wherein `stats::predict(spline_basis, data[[time_observed_continuous]])`
produces an object that can serve as a term in the model.

## Covariance structures

The user specifies covariance structure *candidates* via the
`cov_structs` argument. These structures will be attempted in order
until a model converges successfully.

When any covariance structure other than `"us"` (heterogeneous
unstructured) is used, `"Empirical-Bias-Reduced"` is passed to
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
as the `vcov` argument (see
[`mmrm::mmrm_control()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm_control.html)).

When fitting models, these analysis functions specify the covariance
structure through the `covariance` argument of
[`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

## Building the model formula

These analysis functions automatically build the model formula from its
arguments. The user cannot remove any of these auto-generated terms, but
terms can be added via the `covariates` argument.

### Time spline terms

Natural cubic splines will be applied to the `time_observed_continuous`
variable in `data`. These splines will be constructed according to the
user-specified `spline_basis`. A custom `spline_fn()` is constructed
under the hood that accepts `time_observed_continuous` and produces a
spline matrix based on the `spline_basis`. Thus, the model formula
includes a time spline term resembling
`spline_fn(time_observed_continuous)`.

### `arm` and `subgroup` terms

All generated models include an interaction term between the time spline
term and the study `arm` term, but `arm` is not included as a main
effect by default. If this is desired, use the `covariates` argument
(e.g., specify `covariates = ~ arm`).

Concerning
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
the `subgroup` variable is included as a main effect, and its
interaction with the time spline is also included. Furthermore, the
second-order interaction term between the time spline, `subgroup`, and
`arm` is also included for the main analysis model and the "full" model
(when `subgroup_interaction_test = TRUE`; see **Subgroup interaction
test** below).

### Adding terms with `covariates`

The user can specify additional terms through the `covariates` argument,
which must be a formula.

The user cannot specify the covariance structure with this argument. See
the **Covariance structures** section above.

The user can remove the intercept from the model by including `0` as a
term in `covariates`.

### Model formula templates

The model formulas that the analysis functions construct will take the
form of the formula templates below.

#### [`ncs_analysis()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md) (i.e., no subgroup)

    response ~
      spline_fn(time_observed_continuous) +
      spline_fn(time_observed_continuous):arm {+
      covariates}

#### [`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)

Main analysis model and "full" model:

    response ~
      spline_fn(time_observed_continuous) +
      subgroup +
      spline_fn(time_observed_continuous):subgroup +
      spline_fn(time_observed_continuous):arm +
      spline_fn(time_observed_continuous):subgroup:arm {+
      covariates}

"reduced" model:

    response ~
      spline_fn(time_observed_continuous) +
      subgroup +
      spline_fn(time_observed_continuous):subgroup +
      spline_fn(time_observed_continuous):arm {+
      covariates}

### Expanding spline terms

When `expand_spline_terms = TRUE` and `spline_basis` has at least two
[dim](https://rdrr.io/r/base/dim.html)ensions (e.g., if it is a matrix,
which is typical), the spline term will be split into multiple terms:
one for each of its columns.

For instance, if the user specifies a `spline_basis` with 3 degrees of
freedom, the above no-subgroup model formula template would become:

    response ~
      spline_fn(time_observed_continuous)[, 1] +
      spline_fn(time_observed_continuous)[, 2] +
      spline_fn(time_observed_continuous)[, 3] +
      spline_fn(time_observed_continuous)[, 1]:arm +
      spline_fn(time_observed_continuous)[, 2]:arm +
      spline_fn(time_observed_continuous)[, 3]:arm {+
      covariates}

## Examples

``` r
# Create a usable data set out of mmrm::fev_data
fev_mod <- mmrm::fev_data
fev_mod$VISITN <- fev_mod$VISITN * 10
fev_mod$time_cont <- fev_mod$VISITN + rnorm(nrow(fev_mod))
fev_mod$obs_visit_index <- round(fev_mod$time_cont)

# Example without subgroup:
ncs_mmrm_fit(
  data = fev_mod,
  type = "basic",
  response = FEV1,
  subject = USUBJID,
  cov_structs = c("ar1", "us"),
  time_observed_continuous = time_cont,
  df = 2,
  time_observed_index = obs_visit_index,
  arm = ARMCD,
  control_group = "PBO",
  covariates = ~ FEV1_BL + RACE
)
#> In as.ordered(obs_visit_index) there are dropped visits: 27, 33.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.
#> mmrm fit
#> 
#> Formula:     FEV1 ~ spline_fn(time_cont)[, 1] + spline_fn(time_cont)[, 2] +  
#>     FEV1_BL + RACE + spline_fn(time_cont)[, 1]:ARMCD + spline_fn(time_cont)[,  
#>     2]:ARMCD
#> Data:        fev_mod (used 537 observations from 197 subjects with maximum 23 
#> timepoints)
#> Covariance:  auto-regressive order one (2 variance parameters)
#> Inference:   REML
#> Deviance:    3519.085
#> 
#> Coefficients: 
#>                        (Intercept)          spline_fn(time_cont)[, 1] 
#>                         21.9675040                         21.6539457 
#>          spline_fn(time_cont)[, 2]                            FEV1_BL 
#>                         16.2898692                          0.1697069 
#>      RACEBlack or African American                          RACEWhite 
#>                          0.4284112                          5.2781845 
#> spline_fn(time_cont)[, 1]:ARMCDTRT spline_fn(time_cont)[, 2]:ARMCDTRT 
#>                          7.5780942                          0.3654192 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch

# Example with subgroup:
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
#> In as.ordered(obs_visit_index) there are dropped visits: 27, 33.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.
#> mmrm fit
#> 
#> Formula:     FEV1 ~ spline_fn(time_cont)[, 1] + spline_fn(time_cont)[, 2] +  
#>     SEX + FEV1_BL + RACE + spline_fn(time_cont)[, 1]:SEX + spline_fn(time_cont)[,  
#>     2]:SEX + spline_fn(time_cont)[, 1]:ARMCD + spline_fn(time_cont)[,  
#>     2]:ARMCD + spline_fn(time_cont)[, 1]:SEX:ARMCD + spline_fn(time_cont)[,  
#>     2]:SEX:ARMCD
#> Data:        fev_mod (used 537 observations from 197 subjects with maximum 23 
#> timepoints)
#> Covariance:  auto-regressive order one (2 variance parameters)
#> Inference:   REML
#> Deviance:    3495.243
#> 
#> Coefficients: 
#>                                  (Intercept) 
#>                                  19.04064075 
#>                    spline_fn(time_cont)[, 1] 
#>                                  26.03036523 
#>                    spline_fn(time_cont)[, 2] 
#>                                  16.16544536 
#>                                    SEXFemale 
#>                                   4.84326016 
#>                                      FEV1_BL 
#>                                   0.17108349 
#>                RACEBlack or African American 
#>                                   0.62450086 
#>                                    RACEWhite 
#>                                   5.45148575 
#>          spline_fn(time_cont)[, 1]:SEXFemale 
#>                                  -7.52228108 
#>          spline_fn(time_cont)[, 2]:SEXFemale 
#>                                   0.36217537 
#>           spline_fn(time_cont)[, 1]:ARMCDTRT 
#>                                  10.16711616 
#>           spline_fn(time_cont)[, 2]:ARMCDTRT 
#>                                   0.01344647 
#> spline_fn(time_cont)[, 1]:SEXFemale:ARMCDTRT 
#>                                  -4.60721973 
#> spline_fn(time_cont)[, 2]:SEXFemale:ARMCDTRT 
#>                                   0.38589869 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```
