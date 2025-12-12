# `splinetrials_subgroup_analysis` object

[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)
returns an object of class `splinetrials_subgroup_analysis`: a named
[list](https://rdrr.io/r/base/list.html) with three to seven elements.

## `between` and `within`

These are each
[`tibbles`](https://dplyr.tidyverse.org/reference/reexports.html), and
they share many of the same columns and values but are sorted in a
different order. Each contains one row per unique combination of `arm`,
`time_scheduled_label`, and `subgroup` found in the `data` (see the
arguments of
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)).
The values in columns `arm` through `change_p_value` as well as
`correlation` and `optimizer` are identical. The two tables' treatment
effect analysis results columns differ in name and content, with
`between`'s columns bearing the prefix `diff_subgroup_` and `within`'s
columns bearing the prefix `diff_arm_` (see the **Treatment effects**
section of
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md)).
Lastly, only `within` contains the percent slowing analysis results.

### `between`

A 30-column
[`tibble`](https://dplyr.tidyverse.org/reference/reexports.html) sorted
by `time`, then by `arm`, then by `subgroup`.

Columns:

1.  `arm`: values of `data[[arm]]`.

2.  `time`: values of `data[[time_scheduled_label]]`.

3.  `subgroup`: values of `data[[subgroup]]`.

4.  `n`: number of times the combination appears in data.

5.  `est`: [mean](https://rdrr.io/r/base/mean.html) of
    `data[[response]]`.

6.  `sd`: [standard deviation](https://rdrr.io/r/stats/sd.html) of
    `data[[response]]`.

7.  `se`: standard error of `data[[response]]` (i.e., `sd / sqrt(n)`).

8.  `lower`: lower bound of confidence interval.

9.  `upper`: upper bound of confidence interval.

10. `response_est`: estimated marginal mean.

11. `response_se`: standard error of `response_est`.

12. `response_df`: degrees of freedom used to calculate the confidence
    interval for `response_est`.

13. `response_lower`: lower bound of confidence interval for
    `response_est`.

14. `response_upper`: upper bound of confidence interval for
    `response_est`.

15. `change_est`: estimated change from baseline.

16. `change_se`: standard error of `change_est`.

17. `change_df`: degrees of freedom used for calculating the confidence
    interval for and testing the significance of `change_est`.

18. `change_lower`: lower bound of confidence interval for `change_est`.

19. `change_upper`: upper bound of confidence interval for `change_est`.

20. `change_test_statistic`: test statistic measuring the significance
    of `change_est`.

21. `change_p_value`: p-value for the significance of `change_est`.

22. `diff_subgroup_est`: treatment effect of `subgroup` within `arm`.

23. `diff_subgroup_se`: standard error of `diff_subgroup_est`.

24. `diff_subgroup_df`: degrees of freedom used for calculating the
    confidence interval for and testing the significance of
    `diff_subgroup_est`.

25. `diff_subgroup_lower`: lower bound of confidence interval for
    `diff_subgroup_est`.

26. `diff_subgroup_upper`: upper bound of confidence interval for
    `diff_subgroup_est`.

27. `diff_subgroup_test_statistic`: test statistic measuring the
    significance of `diff_subgroup_est`.

28. `diff_subgroup_p_value`: p-value for the significance of
    `diff_subgroup_est`.

29. `correlation`: the covariance structure of the analysis model. This
    is the same value repeated for each row.

30. `optimizer`: invariably `mmrm+tmb` to indicate that
    [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
    (which uses the `TMB` package) was used to fit the model.

## `within`

A 33-column
[`tibble`](https://dplyr.tidyverse.org/reference/reexports.html) sorted
by `subgroup`, then by `arm`, then by `time`.

Columns:

1.  `arm`: values of `data[[arm]]`.

2.  `time`: values of `data[[time_scheduled_label]]`.

3.  `subgroup`: values of `data[[subgroup]]`.

4.  `n`: number of times the combination appears in data.

5.  `est`: [mean](https://rdrr.io/r/base/mean.html) of
    `data[[response]]`.

6.  `sd`: [standard deviation](https://rdrr.io/r/stats/sd.html) of
    `data[[response]]`.

7.  `se`: standard error of `data[[response]]` (i.e., `sd / sqrt(n)`).

8.  `lower`: lower bound of confidence interval.

9.  `upper`: upper bound of confidence interval.

10. `response_est`: estimated marginal mean.

11. `response_se`: standard error of `response_est`.

12. `response_df`: degrees of freedom used for calculating the
    confidence interval for `response_est`.

13. `response_lower`: lower bound of confidence interval for
    `response_est`.

14. `response_upper`: upper bound of confidence interval for
    `response_est`.

15. `change_est`: estimated change from baseline.

16. `change_se`: standard error of `change_est`.

17. `change_df`: degrees of freedom for calculating the confidence
    interval for and estimating the significance of `change_est`.

18. `change_lower`: lower bound of confidence interval for `change_est`.

19. `change_upper`: upper bound of confidence interval for `change_est`.

20. `change_test_statistic`: test statistic measuring the significance
    of `change_est`.

21. `change_p_value`: p-value for the significance of `change_est`.

22. `diff_arm_est`: treatment effect of `arm` within `subgroup`.

23. `diff_arm_se`: standard error of `diff_arm_est`.

24. `diff_arm_df`: degrees of freedom for calculating the confidence
    interval for and testing the significance of `diff_arm_est`.

25. `diff_arm_lower`: lower bound of confidence interval for
    `diff_arm_est`.

26. `diff_arm_upper`: upper bound of confidence interval for
    `diff_arm_est`.

27. `diff_arm_test_statistic`: test statistic measuring the significance
    of `diff_arm_est`.

28. `diff_arm_p_value`: p-value for the significance of `diff_arm_est`.

29. `percent_slowing_est`: estimated percent slowing.

30. `percent_slowing_lower`: lower bound of confidence interval for
    `percent_slowing_est`.

31. `percent_slowing_upper`: upper bound of confidence interval for
    `percent_slowing_est`.

32. `correlation`: the covariance structure of the analysis model. This
    is the same value repeated for each row.

33. `optimizer`: invariably `mmrm+tmb` to indicate that
    [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
    (which uses the `TMB` package) was used to fit the model.

## `type3`

A [`tibble`](https://dplyr.tidyverse.org/reference/reexports.html) with
a row for each term in the model (not counting any intercepts). Contains
the following six columns:

1.  `effect`: the name of the model term.

2.  `chisquare_test_statistic`: the Chi-squared test statistic measuring
    the significance of the model term.

3.  `df`: the degrees of freedom used for testing the significance of
    the model term.

4.  `p_value`: the p-value for the significance of the model term.

5.  `correlation`: the covariance structure of the analysis model. This
    is the same value repeated for each row.

6.  `optimizer`: invariably `mmrm+tmb` to indicate that
    [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
    (which uses the `TMB` package) was used to fit the model.

## `interaction`

This element is only present if `subgroup_interaction_test = TRUE`.

A 2 by 10 data frame with class `anova.mmrm`. The first row represents
the "reduced" model and the second row represents the "full" model. The
columns are as follows:

1.  `model`: `c("reduced model", "full model")`, identifying the model
    associated with each row.

2.  `aic`: the [AIC](https://rdrr.io/r/stats/AIC.html) of the model.

3.  `bic`: the [BIC](https://rdrr.io/r/stats/AIC.html) of the model.

4.  `loglik`: the [log likelihood](https://rdrr.io/r/stats/logLik.html)
    of the model.

5.  `-2*log(l)`: equal to `-2 * loglik`.

6.  `test_statistic`: the test statistic used for testing the
    significance of the second-order interaction term(s) between the
    spline time, `subgroup`, and `arm`. This value is the second element
    of the column; the first element is always a missing value.

7.  `df`: the degrees of freedom used for testing the significance of
    the second-order interaction term(s) between the spline term,
    `subgroup`, and `arm`. This value is the second element of the
    column; the first element is always a missing value.

8.  `p_value`: the p-value for the significance of the second-order
    interaction term(s) between the spline term, `subgroup`, and `arm`.
    This value is the second element of the column; the first element is
    always a missing value.

9.  `correlation`: the covariance structure of the analysis model. This
    is the same value repeated for each row.

10. `optimizer`: invariably `mmrm+tmb` to indicate that
    [`mmrm::mmrm()`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
    (which uses the `TMB` package) was used to fit the model.

## `analysis_model`

This element is only present if `return_models = TRUE`.

An
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
object: the fitted model used to perform analyses that produced the
`between`, `within`, and `type3` results.

## `full` and `reduced`

These elements are only present if `subgroup_interaction_test = TRUE`
and `return_models = TRUE`.

Both are
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
objects: the two maximum-likelihood-estimated models used to perform the
subgroup interaction test whose results are in the `interaction`
element. See the **Subgroup interaction test** section of
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md).

## See also

The function
[`ncs_analysis_subgroup()`](https://nikkrieger.github.io/splinetrials/reference/ncs_analysis_subgroup.md),
which produces objects of this class.
