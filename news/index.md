# Changelog

## splinetrials 0.1.1

- Catch novel non-convergence error. Not catching these had made it so
  that model fitting would just quit because the program thought it was
  a different kind of error the user had to figure out (e.g., syntax).
- Removed errant time_scheduled_continuous argument in
  [`ncs_mmrm_fit()`](https://nikkrieger.github.io/splinetrials/reference/ncs_mmrm_fit.md).
- Improved pkgdown site, categorizing functions in the function
  reference.
- Improved README, basing it on
  [`survival::pbcseq`](https://rdrr.io/pkg/survival/man/pbcseq.html)
  instead of randomly generated data.

## splinetrials 0.1.0

CRAN release: 2025-12-18

- Initial CRAN submission.
