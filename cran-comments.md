## Resubmission

This is a resubmission to optimize MLwrap 0.2.3.

In this new version 0.3.0.:

A bug in sensitivity_analysis() caused failures when using probability-based
metrics like roc_auc. The issue was tidyselect failing to evaluate column names
in yardstick::roc_auc(). Fixed by using yardstick::roc_auc_vec() instead, which
accepts vectors directly. This eliminates the tidyselect dependency for PFI
calculations.

This version introduces global and pairwise interaction analysis based on
Friedman’s H-statistic, providing normalized H2 measures for both overall
feature non-additivity and specific feature pairs across regression, binary,
and multiclass tasks. These new functions compute interaction strength from
partial dependence structures and return both tabular summaries and dedicated
plotting utilities, allowing users to systematically explore and visualize
complex interaction effects in fitted models.

Code quality improvements have been made to ensure full compliance
with R package standards, including the resolution of non-standard evaluation
warnings detected by R CMD check.

R CMD check results

Local (Windows 11, R 4.5.2): 0 errors \| 0 warnings \| 0 notes,

Optional pretests:

Debian GCC (R-hub) clean, with the expected clock NOTE only.

Additional comments

Thank you for reviewing this resubmission to optimize MLwrap.
