## Resubmission

This is a resubmission to optimize MLwrap 0.2.2.

In this new version 0.2.3.:

The Permutation Feature Importance (PFI) algorithm has been implemented
natively within MLwrap, eliminating the dependency on the 'vip' package.
This change reduces the package's dependency footprint and improves
maintainability while preserving full functional equivalence with the
previous implementation. The PFI algorithm now directly leverages MLwrap's
internal metric system ('metrics_info' and 'yardstick' wrapper functions),
ensuring seamless integration with the package's existing infrastructure.
From the user perspective, the output structure, parameter names, and
computational results remain completely unchanged—users will not need to
modify any existing code that calls sensitivity_analysis() or related
functions. The implementation correctly handles regression, binary
classification, and multiclass classification tasks, and properly accounts
for metric directionality (minimize vs. maximize) across all supported
metric types. Code quality improvements have been made to ensure full
compliance with R package standards, including the resolution of
non-standard evaluation warnings detected by R CMD check.

R CMD check results

Local (Windows 11, R 4.5.2): 0 errors \| 0 warnings \| 0 notes,

Optional pretests:

Debian GCC (R-hub) clean, with the expected clock NOTE only.

Additional comments

Thank you for reviewing this resubmission to optimize MLwrap.
