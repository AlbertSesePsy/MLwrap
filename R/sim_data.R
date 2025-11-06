#' sim_data
#'
#' This dataset, included in the MLwrap package, is a simulated dataset
#' (Martínez-García et al., 2025) designed to capture relationships among
#' psychological and demographic variables influencing psychological wellbeing,
#' the primary outcome variable. It comprises data for 1,000 individuals.
#'
#' The predictor variables include gender (50.7% female), age (range: 18-85
#' years, mean = 51.63, median = 52, SD = 17.11), and socioeconomic status,
#' categorized as Low (n = 343), Medium (n = 347), and High (n = 310).
#' Additional predictors (features) are emotional intelligence (range: 24-120,
#' mean = 71.97, median = 71, SD = 23.79), resilience (range: 4-20,
#' mean = 11.93, median = 12, SD = 4.46), life satisfaction (range: 5-35,
#' mean = 20.09, median = 20, SD = 7.42), and depression (range: 0-63,
#' mean = 31.45, median = 32, SD = 14.85). The primary outcome variable is
#' emotional wellbeing, measured on a scale from 0 to 100 (mean = 50.22,
#' median = 49, SD = 24.45).
#'
#' The dataset incorporates correlations as conditions for the simulation.
#' Psychological wellbeing is positively correlated with emotional intelligence
#' (r = 0.50), resilience (r = 0.40), and life satisfaction (r = 0.60),
#' indicating that higher levels of these factors are associated with better
#' emotional health outcomes. Conversely, a strong negative correlation exists
#' between depression and psychological wellbeing (r = -0.80), suggesting that
#' higher depression scores are linked to lower emotional wellbeing. Age shows
#' a slight positive correlation with emotional wellbeing (r = 0.15),
#' reflecting the expectation that older individuals might experience greater
#' emotional stability. Gender and socioeconomic status are included as
#' potential predictors, but the simulation assumes no statistically
#' significant differences in psychological wellbeing across these categories.
#'
#' Additionally, the dataset includes categorical transformations of
#' psychological wellbeing into binary and polytomous formats: a binary version
#' ("Low" = 477, "High" = 523) and a polytomous version with four levels: "Low"
#' (n = 161), "Somewhat" (n = 351), "Quite a bit" (n = 330), and "Very much"
#' (n = 158). The polytomous transformation uses the 25th, 50th, and 75th
#' percentiles as thresholds for categorizing psychological wellbeing scores.
#' These transformations enable analyses using machine learning models for
#' regression (continuous outcome) and classification (binary or polytomous
#' outcomes) tasks.
#' @docType data
#' @name sim_data
#' @usage data(sim_data)
#' @format A data frame with 1,000 rows and 10 columns:
#' \describe{
#'   \item{psych_well}{Psychological Wellbeing Indicator. Continuous with (0,100)}
#'   \item{psych_well_bin}{Psychological Wellbeing Binary Indicator. Factor with ("Low", "High")}
#'   \item{psych_well_pol}{Psychological Wellbeing Polytomic Indicator. Factor with ("Low", "Somewhat", "Quite a bit", "Very Much")}
#'   \item{gender}{Patient Gender. Factor ("Female", "Male")}
#'   \item{age}{Patient Age. Continuous (18, 85)}
#'   \item{socioec_status}{Socioeconomial Status Indicator. Factor ("Low", "Medium", "High")}
#'   \item{emot_intel}{Emotional Intelligence Indicator. Continuous (24, 120)}
#'   \item{resilience}{Resilience Indicator. Continuous (4, 20)}
#'   \item{depression}{Depression Indicator. Continuous (0, 63)}
#'   \item{life_sat}{Life Satisfaction Indicator. Continuous (5, 35)}
#' }
#' @section Test Performance Exceeding Training Performance:
#'
#' If machine learning models, including SVMs, show better evaluation metrics
#' on the test set than the training set, this anomaly usually signals
#' methodological issues rather than genuine model quality. Typical causes
#' reported in the literature (Hastie et al., 2017) include:
#'
#' \itemize{
#'   \item \strong{Statistical variance in small samples}: Random train-test
#'         splits may produce partitions where the test set contains
#'         easier-to-classify examples by chance, especially with small sample
#'         sizes or difficult tasks (Vabalas et al., 2019; An et al., 2021).
#'   \item \strong{Synthetic data characteristics}: Simulated data may contain
#'         artificial patterns or non-uniform distributions that create easier
#'         test sets compared to training sets.
#'   \item \strong{Excessive regularization}: High regularization parameters
#'         may limit model capacity to fit training data while paradoxically
#'         generalizing better to simpler test patterns, indicating
#'         underfitting.
#'   \item \strong{Train-test contamination}: Preprocessing (scaling,
#'         normalization) performed before train-test split leaks statistical
#'         information from test to train, producing overoptimistic performance
#'         estimates (Kapoor &  Narayanan, 2023).
#'   \item \strong{Kernel-data interaction}: Inappropriate kernel parameters
#'         may create decision boundaries that better fit test distribution
#'         than training distribution.
#' }
#' \strong{MLwrap implementation:}
#' MLwrap's hyperparameter optimization (via Bayesian Optimization or Grid
#' Search CV) implements 5-fold cross-validation during the tuning process,
#' which provides more robust parameter selection than single train-test
#' splits. Users should examine evaluation metrics across both training and
#' test sets, and review diagnostic plots (residuals, predictions) to identify
#' potential distribution differences between partitions. When working with
#' small datasets where partition variability may be substantial, running the
#' complete workflow with different random seeds can help assess the stability
#' of results and conclusions. The \code{sim_data} dataset included in MLwrap
#' is a simulated matrix provided for demonstration purposes only. As
#' synthetic data, it may occasionally exhibit some of these anomalous
#' phenomena (e.g., better test than training performance) due to artificial
#' patterns in the data generation process. Users working with real-world data
#' should always verify results through careful examination of evaluation
#' metrics and diagnostic plots across multiple runs.
#' @references
#' An, C., Park, Y. W., Ahn, S. S., Han, K., Kim, H., & Lee, S. K. (2021).
#' Radiomics machine learning study with a small sample size: Single random
#' training-test set split may lead to unreliable results. \emph{PLOS ONE},
#' \emph{16}(8), e0256152. \doi{10.1371/journal.pone.0256152}
#'
#' Hastie, T., Tibshirani, R., & Friedman, J. (2017). \emph{The elements of
#' statistical learning: Data mining, inference, and prediction} (2nd ed.,
#' corrected 12th printing, Chapter 7). Springer.
#' \doi{10.1007/978-0-387-84858-7}
#'
#' Kapoor, S., & Narayanan, A. (2023). Leakage and the reproducibility crisis
#' in machine-learning-based science. \emph{Patterns}, \emph{4}(9), 100804.
#' \doi{10.1016/j.patter.2023.100804}
#'
#' Martínez-García, J., Montaño, J. J., Jiménez, R., Gervilla, E.,
#' Cajal, B., Núñez, A., Leguizamo, F., & Sesé, A. (2025).
#' Decoding Artificial Intelligence: A Tutorial on Neural Networks
#' in Behavioral Research. \emph{Clinical and Health, 36}(2), 77-95.
#' \doi{10.5093/clh2025a13}
#'
#' Vabalas, A., Gowen, E., Poliakoff, E., & Casson, A. J. (2019). Machine
#' learning algorithm validation with a limited sample size. \emph{PLOS ONE},
#' \emph{14}(11), e0224365. \doi{10.1371/journal.pone.0224365}
#'
NULL


