# splinetrials (development version)

* Catch novel non-convergence error. Not catching these had made it so that model fitting would just quit because the program thought it was a different kind of error the user had to figure out (e.g., syntax).
* Removed errant time_scheduled_continuous argument in ncs_mmrm_fit()

# splinetrials 0.1.0

* Initial CRAN submission.
