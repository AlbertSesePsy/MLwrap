## Resubmission

This is a resubmission to optimize MLwrap 0.2.1.

In this new version 0.2.2.:

This release adds a Reproducibility section to the `build_model()`
function documenting seed management across algorithms. It clarifies
that `set.seed()` ensures within-computer reproducibility for all
models, and `torch::torch_manual_seed()` is required for Neural
Networks. Cross-computer reproducibility varies by algorithm: Random
Forest and SVM are fully reproducible, XGBoost shows minor numerical
differences, and Neural Networks are statistically consistent with
expected hardware-dependent variations. This documentation helps users
understand and properly manage reproducibility in their analyses.

This release addresses a compatibility issue with torch version 0.16.1 in
the Integrated Gradients sensitivity analysis. The fix converts neural network
models to float64 precision and explicitly specifies dtype = "double" in
innsight::convert() calls, ensuring compatibility with both torch 0.16.0 and
0.16.1 without affecting other functionality.

It also includes enhancements to the package documentation with particular
emphasis on best practices for machine learning workflows. A comprehensive
tutorial document demonstrating the complete MLwrap workflow (from data
preprocessing through sensitivity analysis) with practical examples and
detailed explanations is recommended for users, especially those new to
machine learning or the KDD framework. Such a tutorial document will be
invaluable for practitioners seeking to understand the full capabilities of
the package across different machine learning algorithms (Neural Networks,
Random Forests, SVM, and XGBoost) and to leverage the package's integrated
approach to model evaluation and interpretation.

R CMD check results

Local (Windows 11, R 4.5.0): 0 errors \| 0 warnings \| 0 notes,

Optional pretests:

Debian GCC (R-hub) clean, with the expected clock NOTE only.

Additional comments

Thank you for reviewing this resubmission to optimize MLwrap.
