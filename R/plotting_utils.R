### Tuner Plots ###

#' Plotting Tuner Search Results
#'
#' @description
#'
#' The **plot_tuning_results()** function generates graphical representations of
#' hyperparameter search results, automatically adapting to the type of optimizer
#' used. When Bayesian optimization is employed, the function presents additional
#' plots showing the iterative evolution of the loss function and search results
#' throughout the optimization process. This function validates that model fitting
#' has been completed and that hyperparameter tuning was actually performed before
#' attempting to display results.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the plot with tuning results the user needs to complete till
#' # fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_tuning_results(wrap_object)
#'
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
#' Plots the training loss curve of the Neural Network model on the validation
#' set. This plot can be used for underfitting / overfitting diagnostics.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @examples
#'
#' # Note: For obtaining the loss curve plot the user needs to
#' # complete till the fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#'   # See the full pipeline example under table_best_hyperparameters()
#'   # (Neural Network engine required)
#'   # Final call signature:
#'   # plot_loss_curve(wrap_object)
#'
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
#' Plots a graph visualization of the Neural Network's architecture along with
#' its optimized hyperparameters.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @examples
#' # Note: For obtaining the Neural Network architecture graph plot the user needs
#' # to complete till the fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#'   # See the full pipeline example under table_best_hyperparameters()
#'   # (Neural Network engine required)
#'   # Final call signature:
#'   # plot_graph_nn(wrap_object)
#'
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
#' The **plot_residuals_distribution()** function generates histograms of residual
#' distributions for both training and test data in regression problems. This
#' visualization enables evaluation of error normality and detection of systematic
#' patterns in model residuals. The function uses patchwork to combine training
#' and test plots in a single display for direct comparison.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the residuals distribution plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_residuals_distribution(wrap_object)
#'
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
#' The **plot_scatter_residuals()** function produces scatter plots relating residuals to predictions, facilitating
#' identification of heteroscedasticity and non-linear patterns in model errors. This diagnostic plot is essential
#' for validating regression model assumptions and detecting potential issues with model specification or data quality.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the residuals vs. predicted values plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_scatter_residuals(wrap_object)
#'
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
#' The **plot_scatter_predictions()** function generates scatter plots between
#' observed and predicted values, providing direct visual assessment of model
#' predictive accuracy. The function displays both training and test results side
#' by side, enabling evaluation of model generalization performance and
#' identification of potential overfitting.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the observed vs. predicted values plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # plot_scatter_predictions(wrap_object)
#'
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
#' The **plot_confusion_matrix()** function generates confusion matrices for both
#' training and test data in classification problems. This visualization allows
#' evaluation of classification accuracy by category and identification of
#' confusion patterns between classes, providing insights into which classes are
#' most frequently misclassified.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining confusion matrix plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with categorical outcome.
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_confusion_matrix(wrap_object)
#'
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
#' The **plot_roc_curve()** function produces ROC (Receiver Operating
#' Characteristic) curves, providing fundamental visual metrics for evaluating
#' binary and multiclass classifier performance. The ROC curve illustrates the
#' trade-off between true positive rate and false positive rate across different
#' classification thresholds.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining roc curve plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with categorical outcome.
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_roc_curve(wrap_object)
#'
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
#' The **plot_pr_curve()** function generates precision-recall curves, which are
#' particularly valuable for evaluating classifier performance on imbalanced
#' datasets. These curves show the relationship between precision and recall
#' across different decision thresholds, complementing ROC curve analysis.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_pr_curve(wrap_object)
#'
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
#' The **plot_gain_curve()** function implements specialized visualizations for
#' evaluating model effectiveness in marketing and case selection contexts. The
#' gain curve shows cumulative gains as a function of population percentile,
#' helping assess how well the model identifies high-value cases in ranked
#' populations.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the gain curve plot the user needs to complete till fine_tuning( ) function
#' # of the MLwrap pipeline and only with categorical outcome.
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_gain_curve(wrap_object)
#'
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
#' The **plot_lift_curve()** function produces lift curves that display the lift
#' factor as a function of population percentile. This visualization is
#' particularly useful for direct marketing applications, showing how much
#' better the model performs compared to random selection at different population
#' segments.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the lift curve plot the user needs to complete till
#' # fine_tuning( ) function of the MLwrap pipeline and only with categorical
#' # outcome.
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_lift_curve(wrap_object)
#'
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
#' The **plot_distribution_by_class()** function generates distributions of
#' model output scores segmented by class, facilitating evaluation of
#' separability between categories and identification of problematic overlaps.
#' This visualization helps assess whether the model produces sufficiently
#' distinct score distributions for different classes.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the distribution by class plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline
#' # and only with categorical outcome.
#'
#' @seealso \code{\link{plot_calibration_curve}}
#'
#' @examples
#' # See the full pipeline example under plot_calibration_curve()
#' # Final call signature:
#' # plot_distribution_by_class(wrap_object)
#'
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
#' The **plot_calibration_curve()** function is specifically designed for binary
#' classification and produces calibration curves that evaluate correspondence
#' between predicted probabilities and observed frequencies. This function is
#' restricted to binary classification problems and provides crucial information
#' about the reliability of the model's probabilistic estimates.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the calibration curve plot the user needs to
#' # complete till fine_tuning( ) function of the MLwrap pipeline and
#' # only with binary outcome.
#'
#' set.seed(123) # For reproducibility
#' wrap_object <- preprocessing(df = sim_data[1:300 ,],
#'                              formula = psych_well_bin ~ depression + resilience,
#'                              task = "classification")
#' wrap_object <- build_model(wrap_object, "Random Forest",
#'                            hyperparameters = list(mtry = 2, trees = 5))
#' wrap_object <- fine_tuning(wrap_object, "Grid Search CV")
#'
#' # And then, you can obtain the calibration curve plot.
#'
#' plot_calibration_curve(wrap_object)
#'
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
#' The **plot_pfi()** function generates bar plots to visualize feature
#' importance through permutation, providing clear representation of each
#' predictor variable's relative contribution to model performance. The
#' function includes an option to display accompanying numerical results
#' tables for comprehensive interpretation.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "PFI")'.
#'
#' @param show_table Boolean. Whether to print PFI results table.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the PFI plot results the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the PFI method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "PFI"))
#' # Final call signature:
#' # plot_pfi(wrap_object)
#'
#' @export
plot_pfi <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  pfi_names   <- grep("^PFI", names(plots), value = TRUE)

  if (length(pfi_names) == 0){

    stop("You need to calculate PFI values first with 'sensitivity_analysis()'!")

  }

  pfi_plots   <- plots[pfi_names]

  combined <- patchwork::wrap_plots(pfi_plots)

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_pfi_results(analysis_object, show_table = TRUE)

    }

    plot(combined)

  }

  invisible(analysis_object)

}

#' Plotting SHAP Plots
#'
#' @description
#'
#' The **plot_shap()** function implements a comprehensive set of visualizations
#' for SHAP values, including bar plots of mean absolute values, directional
#' plots showing positive or negative contribution nature, box plots
#' illustrating SHAP value distributions by variable, and swarm plots combining
#' individual and distributional information. This multifaceted approach enables
#' deep understanding of how each feature influences model predictions.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.
#'
#' @param show_table Boolean. Whether to print SHAP summarized results table.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the SHAP plots the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the SHAP method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "SHAP"))
#' # Final call signature:
#' # plot_shap(wrap_object)
#'
#' @export
plot_shap <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  shap_names   <- grep("^SHAP", names(plots), value = TRUE)

  if (length(shap_names) == 0){

    stop("You need to calculate SHAP values first with 'sensitivity_analysis()'!")

  }

  mean_shap_names <- grep("barplot$", shap_names, value = TRUE)

  dir_shap_names <- grep("directional$", shap_names, value = TRUE)

  box_shap_names <- grep("boxplot$", shap_names, value = TRUE)

  swarm_shap_names <- grep("swarmplot$", shap_names, value = TRUE)

  mean_shap_plots <- plots[mean_shap_names]

  dir_shap_plots <- plots[dir_shap_names]

  box_shap_plots <- plots[box_shap_names]

  swarm_shap_plots <- plots[swarm_shap_names]

  combined_mean <- patchwork::wrap_plots(mean_shap_plots)
  combined_dir <- patchwork::wrap_plots(dir_shap_plots)
  combined_box <- patchwork::wrap_plots(box_shap_plots)
  combined_swarm <- patchwork::wrap_plots(swarm_shap_plots)

  if (base::interactive()){

    if (show_table == TRUE){

    tables <- table_shap_results(analysis_object, show_table = TRUE)

    }

    plot(combined_mean)
    plot(combined_dir)
    plot(combined_box)
    plot(combined_swarm)

    }

  invisible(analysis_object)

}

#' Plotting Integrated Gradients Plots
#'
#' @description
#'
#' The **plot_integrated_gradients()** function replicates the SHAP
#' visualization structure for integrated gradient values, providing the same
#' four graphical modalities adapted to this specific interpretability
#' methodology for neural networks. This function is particularly valuable for
#' understanding feature importance in deep learning architectures where
#' gradients provide direct information about model sensitivity.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Integrated Gradients")'.
#'
#' @param show_table Boolean. Whether to print Integrated Gradients summarized results table.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Integrated Gradients plot the user needs to
#' # complete till sensitivity_analysis( ) function of the MLwrap pipeline
#' # using the Integrated Gradients method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Integrated Gradients"))
#' # Final call signature:
#' # plot_integrated_gradients(wrap_object)
#'
#' @export
plot_integrated_gradients <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  ig_names   <- grep("^Integrated", names(plots), value = TRUE)

  if (length(ig_names) == 0){

    stop("You need to calculate Integrated Gradients values first with 'sensitivity_analysis()'!")

  }

  mean_ig_names <- grep("barplot$", ig_names, value = TRUE)

  dir_ig_names <- grep("directional$", ig_names, value = TRUE)

  box_ig_names <- grep("boxplot$", ig_names, value = TRUE)

  swarm_ig_names <- grep("swarmplot$", ig_names, value = TRUE)

  mean_ig_plots <- plots[mean_ig_names]

  dir_ig_plots <- plots[dir_ig_names]

  box_ig_plots <- plots[box_ig_names]

  swarm_ig_plots <- plots[swarm_ig_names]

  combined_mean <- patchwork::wrap_plots(mean_ig_plots)
  combined_dir <- patchwork::wrap_plots(dir_ig_plots)
  combined_box <- patchwork::wrap_plots(box_ig_plots)
  combined_swarm <- patchwork::wrap_plots(swarm_ig_plots)

  if (base::interactive()){

    if (show_table){

      tables <- table_integrated_gradients_results(analysis_object, show_table = TRUE)

    }

    plot(combined_mean)
    plot(combined_dir)
    plot(combined_box)
    plot(combined_swarm)

  }

  invisible(analysis_object)

}

#' Plotting Olden Values Barplot
#'
#' @description
#'
#' The **plot_olden()** function generates specialized bar plots for visualizing
#' Olden method results, which provide importance measures specific to neural
#' networks based on connection weight analysis. This method offers insights
#' into how input variables influence predictions through the network's synaptic
#' connections.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Olden")'.
#'
#' @param show_table Boolean. Whether to print Olden results table.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Olden plot the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using the Olden
#' # method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Olden"))
#' # Final call signature:
#' # plot_olden(wrap_object)
#'
#' @export
plot_olden <- function(analysis_object, show_table = FALSE){

  plots <- analysis_object$plots

  olden_names   <- grep("^Olden", names(plots), value = TRUE)

  if (length(olden_names) == 0){

    stop("You need to calculate Olden values first with 'sensitivity_analysis()'!")

  }

  olden_plots <- plots[olden_names]

  combined <- patchwork::wrap_plots(olden_plots)

  if (base::interactive()){

    if (show_table == TRUE){

      tables <- table_olden_results(analysis_object, show_table = TRUE)

    }

    plot(combined)

  }

  invisible(analysis_object)

}

#' Plotting Sobol-Jansen Values Barplot
#'
#' @description
#'
#' The **plot_sobol_jansen()** function produces bar plots for Sobol-Jansen
#' analysis results, offering a global sensitivity perspective based on variance
#' decomposition. This methodology is particularly valuable for identifying
#' higher-order effects and complex interactions between variables in model
#' predictions.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Sobol_Jansen")'.
#'
#' @param show_table Boolean. Whether to print Sobol-Jansen results table.
#'
#' @returns analysis_object
#' @examples
#' # Note: For obtaining the Sobol_Jansen plot the user needs to complete till
#' # sensitivity_analysis( ) function of the MLwrap pipeline using
#' # the Sobol_Jansen method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis()
#' # (Requires sensitivity_analysis(methods = "Sobol_Jansen"))
#' # Final call signature:
#' # plot_sobol_jansen(wrap_object)
#'
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
