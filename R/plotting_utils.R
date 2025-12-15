### Tuner Plots ###

#' Plotting Tuner Search Results
#'
#' @description
#'
#' The **plot_tuning_results()** function Visualizes hyperparameter optimization
#' search results adapting output format to the optimization methodology
#' employed. For Bayesian Optimization: displays iteration-by-iteration loss
#' function evolution across iterations, acquisition function values guiding
#' sequential hyperparameter sampling, and final hyperparameter configuration
#' with cross-validation performance metrics. For Grid Search: displays
#' performance surfaces across hyperparameter dimensions and rank-ordered
#' configurations by validation performance.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the plot with tuning results the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_tuning_results(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_tuning_results <- function(analysis_object){

  if ((analysis_object$stage != "fit_model") && (analysis_object$stage != "evaluated_model")){

    stop("You must first fit a model with 'fine_tuning()'!")

  }

  if (analysis_object$hyperparameters$tuning == FALSE){

    stop("All hyperparameters are fixed values, no tuning was performed!")

  }

  plots <- analysis_object$plots

  if (analysis_object$tuner == "Bayesian Optimization"){

    plot(plots$bayesian_opt_iter_loss)

    plot(plots$bayesian_opt_iter_results)

  }

  plot(plots$tuner_search_results)

  invisible(analysis_object)

}

#' Plot Neural Network Loss Curve
#'
#' @description
#'
#' Displays training loss trajectory computed on the validation set across
#' training epochs. Enables visual diagnosis of convergence dynamics,
#' identification of appropriate early stopping points, detection of overfitting
#' patterns (where validation loss increases while training loss decreases), and
#' assessment of optimization stability throughout the training process.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the loss curve plot the user needs to
#' # complete till the fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # (Neural Network engine required)
#' # Final call signature:
#' # plot_loss_curve(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_loss_curve <- function(analysis_object){

  if ((analysis_object$stage != "fit_model") && (analysis_object$stage != "evaluated_model")){

    stop("You must first fit a Neural Network model with 'fine_tuning()'!")

  }

  if (analysis_object$model_name != "Neural Network"){

    stop("Loss curve is only available for Neural Network models!")

  }

  plots <- analysis_object$plots

  p <- plots$nn_loss_curve

  plot(p)

}

#' Plot Neural Network Architecture
#'
#' @description
#'
#' Renders a directed acyclic graph representation of Neural Network
#' architecture showing layer stacking order, layer-specific dimensions
#' (neurons per layer), activation functions applied at each layer, and
#' optimized hyperparameter values (learning rate, batch size, dropout rates,
#' regularization coefficients) obtained from hyperparameter tuning procedures.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Neural Network architecture graph plot the user
#' # needs to complete till the fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # (Neural Network engine required)
#' # Final call signature:
#' # plot_graph_nn(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_graph_nn <- function(analysis_object){

  if (analysis_object$model_name != "Neural Network"){

    stop("Model should be 'Neural Network'!")

  }

  if (analysis_object$stage != "fit_model"){

    stop("You first need to fit a model with 'fine_tuning()'!")

  }

  final_model <- analysis_object$final_model

  model_parsnip <- tune::extract_fit_parsnip(final_model)

  p <- graph_nn(model_parsnip)

  print(p)

  invisible(analysis_object)

}

### Regression Plots ###

#' Plotting Residuals Distribution
#'
#' @description
#'
#' The **plot_residuals_distribution()** function generates histogram and kernel
#' density visualizations of residuals for regression models on training and
#' test datasets. Enables assessment of residual normality through visual
#' inspection of histogram shape, detection of systematic biases indicating
#' omitted variables or model specification errors, and identification of heavy
#' tails suggesting outliers or influential observations.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the residuals distribution plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_residuals_distribution(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_residuals_distribution <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$residuals_dist_train

  p_test <- plots$residuals_dist_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

  }

#' Plotting Residuals vs Predictions
#'
#' @description
#'
#' The **plot_scatter_residuals()** function Visualizes residuals plotted
#' against fitted values to detect violations of ordinary least squares
#' assumptions including homoscedasticity (constant error variance), linearity,
#' and independence. Identifies heteroscedastic patterns (non-constant variance
#' across the predictor range), systematic curvature indicating omitted
#' polynomial terms, and outlier points with extreme residual magnitudes.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the residuals vs. predicted values plot the user needs
#' # to complete till fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_scatter_residuals(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_scatter_residuals <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$scatter_residuals_train

  p_test <- plots$scatter_residuals_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))
  invisible(analysis_object)

}

#' Plotting Observed vs Predictions
#'
#' @description
#'
#' The **plot_scatter_predictions()** function generates scatter plots with
#' 45-degree reference lines comparing observed values (vertical axis) against
#' model predictions (horizontal axis) for training and test data. Enables
#' visual assessment of prediction accuracy through distance from the reference
#' line, identification of systematic bias patterns, detection of
#' heteroscedastic prediction errors, and quantification of generalization
#' performance gaps between training and test sets.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the observed vs. predicted values plot the user needs
#' # to complete till fine_tuning( ) function of the MLwrap pipeline.
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_scatter_predictions(wrap_object)
#' @seealso \code{\link{table_best_hyperparameters}}
#' @export
plot_scatter_predictions <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_regression_plot(analysis_object)

  p_train <- plots$scatter_predictions_train

  p_test <- plots$scatter_predictions_test

  plot(patchwork::wrap_plots(p_train, p_test, ncol = 2))

  invisible(analysis_object)

}

### Classification Plots ###

#' Plotting Confusion Matrix
#'
#' @description
#'
#' The **plot_confusion_matrix()** function generates confusion matrices from
#' classification predictions displaying the contingency table of true class
#' labels versus predicted class labels. Visualizes true positives, true
#' negatives, false positives, and false negatives for both training and test
#' sets, enabling computation of derived performance metrics (sensitivity,
#' specificity, precision, F1-score) and identification of specific class pair
#' misclassification patterns.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining confusion matrix plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with categorical outcome.
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_confusion_matrix(wrap_object)
#' @seealso \code{\link{plot_calibration_curve}}
#' @export
plot_confusion_matrix <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p_train <- plots$confusion_matrix_train

  p_test <- plots$confusion_matrix_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

#' Plotting ROC Curve
#'
#' @description
#'
#' The **plot_roc_curve()** function plots Receiver Operating Characteristic
#' (ROC) curve displaying true positive rate versus false positive rate across
#' all classification probability thresholds. Computes Area Under Curve (AUC)
#' as an aggregate discrimination performance metric independent of threshold
#' selection, providing comprehensive assessment of classifier discrimination
#' ability across the entire decision boundary range.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining roc curve plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with categorical outcome.
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_roc_curve(wrap_object)
#' @seealso \code{\link{plot_calibration_curve}}
#' @export
plot_roc_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$roc_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Precision-Recall Curve
#'
#' @description
#'
#' The **plot_pr_curve()** function generates Precision-Recall curve tracing
#' the relationship between precision and recall across all classification
#' probability thresholds. Particularly informative for imbalanced datasets
#' where ROC curves may be misleading, as PR curves remain sensitive to class
#' distribution changes and provide intuitive performance assessment when one
#' class is substantially rarer than the other.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @seealso \code{\link{plot_calibration_curve}}
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_pr_curve(wrap_object)
#' @export
plot_pr_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$pr_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Gain Curve
#'
#' @description
#'
#' The **plot_gain_curve()** plots cumulative gain as a function of sorted
#' population percentile when observations are ranked by descending predicted
#' probability. For each percentile threshold, calculates the ratio of positive
#' class proportion in the top-ranked subset relative to overall positive class
#' proportion, quantifying model's efficiency in concentrating target cases at
#' the top of rankings.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the gain curve plot the user needs to complete till
#' # fine_tuning( ) function of the MLwrap pipeline and only with categorical
#' # outcome.
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_gain_curve(wrap_object)
#' @seealso \code{\link{plot_calibration_curve}}
#' @export
plot_gain_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$gain_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Lift Curve
#'
#' @description
#'
#' The **plot_lift_curve()** function plots lift factor as a function of
#' population percentile when observations are ranked by descending predicted
#' probability. The lift factor quantifies model's ranking efficiency relative
#' to random ordering baseline at each population cumulative segment, showing
#' how much better model selection performs compared to random case selection.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the lift curve plot the user needs to complete till
#' # fine_tuning( ) function of the MLwrap pipeline and only with categorical
#' # outcome.
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_lift_curve(wrap_object)
#' @seealso \code{\link{plot_calibration_curve}}
#' @export
plot_lift_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p <- plots$lift_curve

  plot(p)

  invisible(analysis_object)

}

#' Plotting Output Distribution By Class
#'
#' @description
#'
#' The **plot_distribution_by_class()** function visualizes kernel density
#' estimates or histograms of predicted probability distributions stratified by
#' true class labels. Enables assessment of class separability through
#' probability overlap quantification and identification of prediction
#' probability ranges where different classes exhibit substantial overlap,
#' indicating classification ambiguity regions.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the distribution by class plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline
#' # and only with categorical outcome.
#' @seealso \code{\link{plot_calibration_curve}}
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_distribution_by_class(wrap_object)
#' @export
plot_distribution_by_class <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  p_train <- plots$dist_by_class_train

  p_test <- plots$dist_by_class_test

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

#' Plotting Calibration Curve
#'
#' @name plot_calibration_curve
#' @aliases plot_calibration_curve
#'
#' @description
#'
#' The **plot_calibration_curve()** function generates calibration plots for
#' binary classification models evaluating the agreement between predicted
#' probabilities and observed class frequencies in binned prediction intervals.
#' Implements reliability diagrams comparing empirical success rates within
#' each probability bin against the predicted probability levels, identifying
#' systematic calibration errors including overconfidence (predicted
#' probabilities exceed observed frequencies) and underconfidence patterns
#' across prediction ranges.
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the calibration curve plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with binary outcome.
#'
#' wrap_object <- preprocessing(df = sim_data[1:300 ,],
#'                              formula = psych_well_bin ~ depression + resilience,
#'                              task = "classification")
#' wrap_object <- build_model(wrap_object, "Random Forest",
#'                            hyperparameters = list(mtry = 2, trees = 5))
#' set.seed(123) # For reproducibility
#' wrap_object <- fine_tuning(wrap_object, "Grid Search CV")
#'
#' # And then, you can obtain the calibration curve plot.
#'
#' plot_calibration_curve(wrap_object)
#' @export
plot_calibration_curve <- function(analysis_object){

  plots <- analysis_object$plots

  check_args_classification_plot(analysis_object)

  if (analysis_object$outcome_levels > 2){

    stop("Calibration Curve is only implemented for binary classification!")

  }

  p_train <- plots$reliability_plot_train + ggplot2::ggtitle("Reliability Plot (train data)")

  p_test  <- plots$reliability_plot_test  + ggplot2::ggtitle("Reliability Plot (test data)")

  plot(patchwork::wrap_plots(p_train, p_test, nrow = 2))

  invisible(analysis_object)

}

#' Plotting Permutation Feature Importance Barplot
#'
#' @description
#'
#' The **plot_pfi()** function generates feature importance estimates via
#' Permutation Feature Importance measuring performance degradation when each
#' feature's values are randomly permuted while holding all other features
#' constant. Provides model-agnostic importance ranking independent of
#' feature-target correlation patterns, capturing both linear and non-linear
#' predictive contributions to model performance.
#'
#' @param analysis_object Fitted analysis_object with
#' 'sensitivity_analysis(methods = "PFI")'.
#' @param show_table Boolean. Whether to print PFI results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the PFI plot results the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the PFI
#' # method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "PFI"))
#' # Final call signature:
#' # plot_pfi(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export
plot_pfi <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  pfi_names   <- grep("^PFI", names(plots), value = TRUE)

  if (length(pfi_names) == 0){

    stop("You need to calculate PFI values first with 'sensitivity_analysis()'!")

  }

  pfi_plot <- plots[["PFI_barplot"]]

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_pfi_results(analysis_object, show_table = TRUE)

    }

    plot(pfi_plot)

  }

  invisible(analysis_object)

}

#' Plotting SHAP Plots
#'
#' @description
#'
#' The **plot_shap()** function implements comprehensive SHAP (SHapley Additive
#' exPlanations) value visualizations where SHAP values represent each
#' feature's marginal contribution to model output based on cooperative game
#' theory principles. Provides four visualization modalities: bar plots of mean
#' absolute SHAP values ranking features by average impact magnitude,
#' directional plots showing feature-value correlation with SHAP magnitude and
#' sign, box plots illustrating SHAP value distributions across instances, and
#' swarm plots combining individual prediction contributions with
#' distributional information.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.
#' @param show_table Boolean. Whether to print SHAP summarized results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the SHAP plots the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the SHAP
#' # method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "SHAP"))
#' # Final call signature:
#' # plot_shap(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export
plot_shap <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  shap_names   <- grep("^SHAP", names(plots), value = TRUE)

  if (length(shap_names) == 0){

    stop("You need to calculate SHAP values first with 'sensitivity_analysis()'!")

  }

  mean_shap_plots <- plots[["SHAP_barplot"]]

  dir_shap_plots <- plots[["SHAP_directional"]]

  box_shap_plots <- plots[["SHAP_boxplot"]]

  swarm_shap_plots <- plots[["SHAP_swarmplot"]]

  if (base::interactive()){

    if (show_table == TRUE){

    tables <- table_shap_results(analysis_object, show_table = TRUE)

    }

    plot(mean_shap_plots)
    plot(dir_shap_plots)
    plot(box_shap_plots)
    plot(swarm_shap_plots)

    }

  invisible(analysis_object)

}

#' Plotting Integrated Gradients Plots
#'
#' @description
#'
#' The **plot_integrated_gradients()** function implements interpretability
#' visualizations of integrated gradient attributions measuring feature
#' importance through accumulated gradients along the interpolation path from
#' baseline (zero vector) to observed input. Provides four visualization
#' modalities: mean absolute attributions (bar plots), directional effects
#' showing positive and negative contribution patterns (directional plots),
#' distributional properties of attributions across instances (box plots),
#' and individual-level attribution contributions (swarm plots).
#'
#' @param analysis_object Fitted analysis_object with
#' 'sensitivity_analysis(methods = "Integrated Gradients")'.
#' @param show_table Boolean. Whether to print Integrated Gradients summarized
#' results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Integrated Gradients plot the user needs to
#' # complete till sensitivity_analysis( ) function of the MLwrap pipeline
#' # using the Integrated Gradients method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Integrated Gradients"))
#' # Final call signature:
#' # plot_integrated_gradients(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export
plot_integrated_gradients <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  ig_names   <- grep("^Integrated", names(plots), value = TRUE)

  if (length(ig_names) == 0){

    stop("You need to calculate Integrated Gradients values first with 'sensitivity_analysis()'!")

  }

  mean_ig_plots <- plots[["IntegratedGradients_barplot"]]

  dir_ig_plots <- plots[["IntegratedGradients_directional"]]

  box_ig_plots <- plots[["IntegratedGradients_boxplot"]]

  swarm_ig_plots <- plots[["IntegratedGradients_swarmplot"]]

  if (base::interactive()){

    if (show_table){

      tables <- table_integrated_gradients_results(analysis_object, show_table = TRUE)

    }

    plot(mean_ig_plots)
    plot(dir_ig_plots)
    plot(box_ig_plots)
    plot(swarm_ig_plots)

  }

  invisible(analysis_object)

}

#' Plotting Olden Values Barplot
#'
#' @description
#'
#' The **plot_olden()** function visualizes Olden sensitivity values computed
#' from products of input-to-hidden layer connection weights and
#' hidden-to-output layer connection weights for each feature. Provides relative
#' feature importance rankings specific to feedforward Neural Networks based on
#' synaptic weight magnitude and directionality analysis across network layers.
#'
#' @param analysis_object Fitted analysis_object with
#' 'sensitivity_analysis(methods = "Olden")'.
#' @param show_table Boolean. Whether to print Olden results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Olden plot the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the Olden
#' # method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Olden"))
#' # Final call signature:
#' # plot_olden(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export
plot_olden <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  olden_names   <- grep("^Olden", names(plots), value = TRUE)

  if (length(olden_names) == 0){

    stop("You need to calculate Olden values first with 'sensitivity_analysis()'!")

  }

  olden_plots <- plots[[olden_names]]

  #combined <- patchwork::wrap_plots(olden_plots)

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_olden_results(analysis_object, show_table = TRUE)

    }

    plot(olden_plots)

  }

  invisible(analysis_object)

}

#' Plotting Sobol-Jansen Values Barplot
#'
#' @description
#'
#' The **plot_sobol_jansen()** function displays first-order and total-order
#' Sobol indices decomposing total output variance into contributions from
#' individual features and higher-order interaction terms. Implements
#' variance-based global sensitivity analysis providing comprehensive
#' understanding of feature contributions to output uncertainty, with
#' application restricted to continuous predictor variables.
#'
#' @param analysis_object Fitted analysis_object with
#' 'sensitivity_analysis(methods = "Sobol_Jansen")'.
#' @param show_table Boolean. Whether to print Sobol-Jansen results table.
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Sobol_Jansen plot the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using
#' # the Sobol_Jansen method.
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Sobol_Jansen"))
#' # Final call signature:
#' # plot_sobol_jansen(wrap_object)
#' @seealso \code{\link{sensitivity_analysis}}
#' @export
plot_sobol_jansen <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  if (is.null(plots$Sobol_Jansen)){

    stop("You need to calculate Sobol_Jansen values first with 'sensitivity_analysis()'!")

  }

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_sobol_jansen_results(analysis_object, show_table = TRUE)

    }

    plot(plots$Sobol_Jansen)

  }

  invisible(analysis_object)

}

#' Plot Partial Dependence (PDP)
#'
#' @description
#' The **plot_pdp()** function computes and visualizes
#' **Partial Dependence Plots (PDP)** for a selected feature, following the
#' methodology described in *Interpretable Machine Learning* by Christoph Molnar.
#' PDPs show the average effect of a feature on model predictions by
#' marginalizing over the distribution of all other features. Optionally,
#' Individual Conditional Expectation (ICE) curves can be added to visualize
#' heterogeneous effects.
#' @param analysis_object A fitted `wrap_object` with model results or
#' previously computed PDP values.
#' @param feature Character. The continuous feature for which the PDP should be
#' computed.
#' @param group_by Optional character. A variable used to produce grouped PDP
#' curves.
#' @param grid_size Integer. Number of points used to evaluate the PDP
#' (default = 25).
#' @param show_ice Logical. Whether to overlay ICE curves (default = TRUE).
#' @param ice_n Integer. Number of ICE curves to sample if `show_ice = TRUE`
#' (default = 50).
#' @param pdp_line_size Numeric. Line width for the PDP curve (default = 1.1).
#' @param use_test Logical. Compute PDP using the test set instead of the
#' training set (default = FALSE).
#' @param plot Logical. If TRUE, prints the PDP plot and returns `wrap_object`;
#' if FALSE, returns the ggplot object without modifying the object.
#' @returns
#' If `plot = TRUE`, returns the updated `wrap_object` and prints the PDP plot.
#' If `plot = FALSE`, returns a ggplot object containing the PDP
#' (and optionally ICE) visualization.
#' @examples
#' # After fitting model with fine_tuning(wrap_object):
#' # plot_pdp(wrap_object, feature = "age")
#' @seealso \code{\link{sensitivity_analysis}}
#' @references
#' Molnar, C. (2022). *Interpretable Machine Learning*.\cr
#' \url{https://christophm.github.io/interpretable-ml-book/}
#' @export
plot_pdp <- function(analysis_object, feature,
                                         group_by = NULL,
                                         grid_size = 25,
                                         show_ice = TRUE, ice_n = 50,
                                         pdp_line_size = 1.1,
                                         use_test = FALSE,
                                         plot = TRUE){

  model <- analysis_object$final_model
  if (use_test){

    data <- analysis_object$data$raw$test_data

  } else {

    data <- analysis_object$data$raw$train_data

  }
  task <- analysis_object$task
  outcome_levels <- analysis_object$outcome_levels

  # 1) Full ICE (no sampling) using your ice_data() with trimmed grid
  ice_full <- ice_data(
    model = model,
    data = data,
    task = task,
    outcome_levels = outcome_levels,
    feature   = feature,
    grid_size = grid_size,
    group_by  = group_by
  )

  # 2) PDP from full ICE
  if ("pred_class" %in% names(ice_full)) {
    # Multiclass: group by class
    if (is.null(group_by)) {
      pdp_df <- ice_full %>%
        dplyr::group_by(pred_class, feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    } else {
      pdp_df <- ice_full %>%
        dplyr::group_by(pred_class, .data[[group_by]], feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    }
  } else {
    # Regression or binary classification
    if (is.null(group_by)) {
      pdp_df <- ice_full %>%
        dplyr::group_by(feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    } else {
      pdp_df <- ice_full %>%
        dplyr::group_by(.data[[group_by]], feature_value) %>%
        dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
    }
  }

  # 3) If plotting ICE, sample IDs from the full ICE (same dataframe)
  if (isTRUE(show_ice)) {
    sampled_ids <- sample(unique(ice_full$id), size = min(ice_n, length(unique(ice_full$id))))
    ice_plot <- ice_full[ice_full$id %in% sampled_ids, , drop = FALSE]
  }

  # Multiclass PDP + ICE facet version
  if ("pred_class" %in% names(ice_full)) {
    p <- ggplot2::ggplot()

    if (isTRUE(show_ice)) {
      p <- p +
        ggplot2::geom_line(data = ice_plot,
                           ggplot2::aes(x = feature_value, y = prediction,
                                        group = interaction(id, pred_class),
                                        color = if (!is.null(group_by)) .data[[group_by]] else "dodgerblue2"),
                           alpha = 0.3, linewidth = 0.4)
    }

    p <- p +
      ggplot2::geom_line(data = pdp_df,
                         ggplot2::aes(x = feature_value, y = prediction,
                                      color = if (!is.null(group_by)) .data[[group_by]] else "dodgerblue2"),
                         linewidth = pdp_line_size) +
      ggplot2::facet_wrap(~ pred_class) +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
      ggplot2::labs(
        title = if (is.null(group_by)) glue::glue("PD Plot {feature}") else glue::glue("PD Plot {feature} by {group_by}"),
        x = feature, y = "Predicted probability",
        color = if (!is.null(group_by)) group_by else NULL
      ) +
      ggplot2::theme_gray()

    if (is.null(group_by)) {
      p <- p + ggplot2::guides(color = "none")
    }

  } else {
    # Regression or binary
    p <- ggplot2::ggplot()

    if (isTRUE(show_ice)) {
      p <- p +
        ggplot2::geom_line(data = ice_plot,
                           ggplot2::aes(x = feature_value, y = prediction, group = id,
                                        color = if (!is.null(group_by)) .data[[group_by]] else NULL),
                           alpha = 0.3, linewidth = 0.4
        )
    }

    p <- p +
      ggplot2::geom_line(data = pdp_df,
                         ggplot2::aes(x = feature_value, y = prediction,
                                      color = if (!is.null(group_by)) .data[[group_by]] else NULL),
                         linewidth = pdp_line_size) +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
      ggplot2::labs(
        title = if (is.null(group_by)) glue::glue("PD Plot {feature}") else glue::glue("PD Plot {feature} by {group_by}"),
        x = feature, y = "Prediction",
        color = group_by
      ) +
      ggplot2::theme_gray()
  }

  x_data <- data[[feature]]

  p <- p + ggplot2::geom_rug(
    data = data.frame(x = x_data),
    ggplot2::aes(x = x),
    sides = "b",
    inherit.aes = FALSE,
    alpha = 1
  )

  if (use_test){

    p$labels$title <- paste0(p$labels$title, " (Test Data)")

  } else {

    p$labels$title <- paste0(p$labels$title, " (Train Data)")

  }

  if (plot){

    plot(p)

    invisible(analysis_object)

  } else{

    return(p)

  }
}

#' Plot Accumulated Local Effects (ALE)
#'
#' @description
#' The **plot_ale()** function computes and visualizes
#' **Accumulated Local Effects (ALE)** for a selected feature, following the
#' approach described in *Interpretable Machine Learning* by Christoph Molnar.
#' ALE plots quantify how changes in a feature locally influence model
#' predictions, offering a robust alternative to Partial Dependence Plots (PDP)
#' by avoiding extrapolation and handling correlated predictors more reliably.
#' @param analysis_object A fitted `wrap_object` with model results or
#' previously computed ALE values.
#' @param feature Character. Name of the continuous feature for which ALE
#' should be computed.
#' @param group Optional character. A grouping variable to compute grouped ALE
#' curves.
#' @param grid.size Integer. Number of intervals to partition the feature domain
#' (default = 20).
#' @param use_test Logical. If TRUE, ALE is computed using the test set
#' (default = FALSE).
#' @param plot Logical. If TRUE, displays the ALE plot and returns `wrap_object`;
#' if FALSE, returns the ggplot object without modifying the object.
#' @returns
#' If `plot = TRUE`, returns the updated `wrap_object` and prints the ALE plot.
#' If `plot = FALSE`, returns a ggplot object containing the ALE visualization.
#' @examples
#' # After fitting a model with fine_tuning(wrap_object):
#' # plot_ale(wrap_object, feature = "age")
#' @seealso \code{\link{sensitivity_analysis}}
#' @references
#' Molnar, C. (2022). *Interpretable Machine Learning*.\cr
#' \url{https://christophm.github.io/interpretable-ml-book/}
#' @export
plot_ale <- function(analysis_object,feature,
                          group = NULL, grid.size = 20,
                          use_test = FALSE,
                          plot = TRUE) {

  task            <- analysis_object$task
  outcome_levels  <- analysis_object$outcome_levels

  if (use_test){

    train <- analysis_object$data$raw$test_data

  } else {

    train <- analysis_object$data$raw$train_data

  }

  model           <- analysis_object$final_model

  ale_long <- comp_ale(model, train, feature, group = group, task = task,
                       outcome_levels = outcome_levels, K = grid.size)

  # Group

  if (is.null(group)) {

    p <- ggplot2::ggplot(ale_long,
                         ggplot2::aes(x = grid, y = ale)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2)

  } else {

    p <- ggplot2::ggplot(ale_long,
                         ggplot2::aes(x = grid, y = ale, color = Level)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2)
  }

  # Facet wrap

  if (outcome_levels > 2) {
    p <- p + ggplot2::facet_wrap(~ Class)
  }

  # Add rug

  x_data <- train[[feature]]
  p <- p + ggplot2::geom_rug(
    data = data.frame(x = x_data),
    ggplot2::aes(x = x),
    sides = "b",
    inherit.aes = FALSE,
    alpha = 1
  ) +
    ggplot2::scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
    ggplot2::labs(
      x = feature,
      y = "ALE",
      title = paste("ALE Plot of", feature,
                    if (!is.null(group)) paste("grouped by", group))
    ) +
    ggplot2::theme_gray()

  if (use_test){

    p$labels$title <- paste0(p$labels$title, " (Test Data)")

  } else {

    p$labels$title <- paste0(p$labels$title, " (Train Data)")

  }

  if (plot){

    plot(p)

    invisible(analysis_object)

  } else{

    return(p)

  }
}
