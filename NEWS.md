# MLwrap 0.3.0

* Depurated bug on classification tasks for Permutation Feature Importance
  (PFI) estimation
  
* Added global and pairwise interaction analysis based on Friedman’s
  H-statistic, including tables and plots of normalized H2 values to inspect
  overall and pairwise feature interactions across regression and
  classification models.

# MLwrap 0.2.3

* Permutation Feature Importance (PFI) now implemented natively in MLwrap,
  eliminating the vip package dependency. Output and API remain unchanged.

# MLwrap 0.2.2

* Added comprehensive documentation on reproducibility with random seed
  management guidance across all supported algorithms.

* Released complete tutorial for the MLwrap workflow (arXiv preprint).

* Fixed Integrated Gradients compatibility with torch 0.16.1 through
  float64 precision implementation.

# MLwrap 0.2.1

* Updated to ensure full compatibility and integration with the jamovi framework
  for module development using MLwrap functionalities.

# MLwrap 0.2.0

* Cross validation procedure implemented for Bayesian Optimization (fine tuning)

# MLwrap 0.1.1

* Fixed deprecation warnings by replacing `aes_string()` with `aes()` and `sym()`
  with `rlang::sym()`
* Corrected plot title assignment in `plot_calibration_curve()` function
* Minor code improvements for better maintainability

# MLwrap 0.1.0

* Initial CRAN release
