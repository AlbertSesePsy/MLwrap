### Tuning Results
#' Best Hyperparameters Configuration
#'
#' @name table_best_hyperparameters
#' @aliases table_best_hyperparameters
#'
#' @description
#'
#' The **table_best_hyperparameters()** function extracts and presents the optimal
#' hyperparameter configuration identified during the model fine-tuning process.
#' This function validates that the model has been properly trained and that
#' hyperparameter tuning has been performed, combining both constant and optimized
#' hyperparameters to generate a comprehensive table with the configuration that
#' maximizes performance according to the specified primary metric. The function
#' includes optional interactive visualization capabilities through the show_table
#' parameter.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble with best hyperparameter configuration.
#' @examples
#' # Note: For obtaining hyoperparameters table the user needs to
#' # complete till fine_tuning( ) function.
#'
#' set.seed(123) # For reproducibility
#' wrap_object <- preprocessing(df = sim_data[1:300 ,],
#'                              formula = psych_well ~ depression + resilience,
#'                              task = "regression")
#' wrap_object <- build_model(wrap_object, "Random Forest",
#'                            hyperparameters = list(mtry = 2, trees = 3))
#' wrap_object <- fine_tuning(wrap_object, "Grid Search CV")
#'
#' # And then, you can obtain the best hyperparameters table.
#'
#' table_best_hyp <- table_best_hyperparameters(wrap_object)
#'
#' @export
table_best_hyperparameters <- function(analysis_object, show_table = FALSE){

  if (analysis_object$stage != "fit_model"){

    stop("You must first fit a model with 'train_model()'!")

  }

  if (is.null(analysis_object$tuner_fit)){

    stop("All hyperparameters had fixed values, no hyperparameter tuning was performed!")

  }

  best_hyper <- tune::show_best(analysis_object$tuner_fit, metric = analysis_object$metrics[1], n = 1)

  best_hyper <- c(analysis_object$hyperparameters$hyperparams_constant,
                         as.list(best_hyper))

  best_hyp <- tibble::as_tibble(best_hyper)

  if (base::interactive() && show_table == TRUE){

    cli::cat_line()

    cli::cli_h1("Best Hyperparameter Configuration")

    cli::cat_line()

    print(best_hyp)

  }

  invisible(best_hyp)

}


### Results

#' Evaluation Results
#'
#' @description
#'
#' The **table_evaluation_results()** function provides access to trained model
#' evaluation metrics, automatically adapting to the type of problem being analyzed.
#' For binary classification problems, it returns a unified table with performance
#' metrics, while for multiclass classification it generates separate tables for
#' training and test data, enabling comparative performance evaluation and
#' detection of potential overfitting.
#'
#' @param analysis_object Fitted analysis_object with 'fine_tuning()'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with evaluation results.
#' @examples
#' # Note: For obtaining the evaluation table the user needs to
#' # complete till fine_tuning( ) function.
#'
#' @seealso \code{\link{table_best_hyperparameters}}
#'
#' @examples
#' # See the full pipeline example under table_best_hyperparameters()
#' # Final call signature:
#' # table_evaluation_results(wrap_object)
#'
#' @export
table_evaluation_results <- function(analysis_object, show_table = FALSE){

  if (analysis_object$stage != "fit_model"){

    stop("You must first fit a model with 'train_model()'!")

  }

  tables <- analysis_object$tables

  result = list()

  if (analysis_object$outcome_levels > 2){

    result$summary_train <- tibble::as_tibble(tables$summary_train)

    result$summary_test <- tibble::as_tibble(tables$summary_test)

  } else {

    result <- tibble::as_tibble(tables$summary_results)

    result$Dataset <- c("Train", "Test")

    result <- result %>% dplyr::select("Dataset", dplyr::everything())

  }

  if (base::interactive() && show_table){

    cli::cli_h1("Evaluation Results")

    cli::cat_line()

    if (analysis_object$outcome_levels > 2){

      cli::cli_h2("Train Data Evaluation Results")

      print(tables$summary_train)

      cli::cat_line()

      cli::cli_h2("Test Data Evaluation Results")

      print(tables$summary_test)

    } else {

      print(result)


    }

  }

  invisible(result)

}

#### Sensitivity Analysis

#' Permutation Feature Importance Results Table
#'
#' @description
#'
#' The **table_pfi_results()** function extracts Permutation Feature Importance
#' results, a model-agnostic technique that evaluates variable importance through
#' performance degradation when randomly permuting each feature's values.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "PFI")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with PFI results.
#' @examples
#' # Note: For obtaining the table with PFI method results the user needs to
#' # complete till sensitivity_analysis() function of the
#' # MLwrap pipeline using PFI method
#'
#' set.seed(123) # For reproducibility
#' wrap_object <- preprocessing(df = sim_data[1:300 ,],
#'                              formula = psych_well ~ depression + emot_intel,
#'                              task = "regression")
#' wrap_object <- build_model(wrap_object, "Random Forest",
#'                            hyperparameters = list(mtry = 2, trees = 3))
#' wrap_object <- fine_tuning(wrap_object, "Grid Search CV")
#' wrap_object <- sensitivity_analysis(wrap_object, methods = "PFI")
#'
#' # And then, you can obtain the PFI results table.
#'
#' table_pfi <- table_pfi_results(wrap_object)
#'
#' @export
table_pfi_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  pfi_names   <- grep("^PFI", names(tables), value = TRUE)

  if (length(pfi_names) == 0){

    stop("You first need to compute PFI values using 'sensitivity_analysis()'!")

  }

  pfi_tables <- tables[pfi_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Permutation Feature Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(pfi_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", pfi_names[[i]]))

        print(pfi_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(pfi_tables)

    }

  }

  invisible(pfi_tables)

}

#' SHAP Summarized Results Table
#'
#' @description
#'
#' The **table_shap_results()** function processes previously calculated SHAP
#' (SHapley Additive exPlanations) values and generates summarized metrics
#' including mean absolute value, standard deviation of mean absolute value, and
#' a directional sensitivity value calculated as the covariance between feature
#' values and SHAP values divided by the variance of feature values. This
#' directional metric provides information about the nature of the relationship
#' between each variable and model predictions. To summarize the SHAP values
#' calculated, three different metrics are computed:
#'
#' * **Mean Absolute Value**
#' * **Standard Deviation of Mean Absolute Value**
#' * **Directional Sensitivity Value** (Cov(Feature values, SHAP values) / Var(Feature values))
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with SHAP summarized results.
#' @examples
#' # Note: For obtaining the table with SHAP method results the user needs
#' # to complete till sensitivity_analysis() function of the
#' # MLwrap pipeline using the SHAP method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis
#' # (Requires sensitivity_analysis(methods = "SHAP"))
#' # Final call signature:
#' # table_shap_results(wrap_object)
#'
#' @export
table_shap_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  shap_names   <- grep("^SHAP", names(tables), value = TRUE)

  if (length(shap_names) == 0){

    stop("You first need to compute SHAP values using 'sensitivity_analysis()'!")

  }

  shap_tables <- tables[shap_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("SHAP Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(shap_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", shap_names[[i]]))

        print(shap_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(shap_tables)

    }

  }

  invisible(shap_tables)

}

#' Integrated Gradients Summarized Results Table
#'
#' @description
#'
#' The **table_integrated_gradients_results()** function implements the same
#' summarized metrics scheme for Integrated Gradients values, a methodology
#' specifically designed for neural networks that calculates feature importance
#' through gradient integration along paths from a baseline to the current input.
#' To summarize the Integrated Gradients values calculated, three different
#' metrics are computed:
#'
#' * **Mean Absolute Value**
#' * **Standard Deviation of Mean Absolute Value**
#' * **Directional Sensitivity Value** (Cov(Feature values, IG values) / Var(Feature values))
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Integrated Gradients")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#'
#' @returns Tibble or list of tibbles (multiclass classification) with Integrated Gradient summarized results.
#' @examples
#' # Note: For obtaining the table with Integrated Gradients method results
#' # the user needs to complete till sensitivity_analysis() function of the
#' # MLwrap pipeline using the Integrated Gradient method.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis
#' # (Requires sensitivity_analysis(methods = "Integrated Gradients"))
#' # Final call signature:
#' # table_integrated_gradients_results(wrap_object)
#'
#' @export
table_integrated_gradients_results <- function(analysis_object, show_table = FALSE){

  tables <- analysis_object$tables

  ig_names   <- grep("^Integrated", names(tables), value = TRUE)

  if (length(ig_names) == 0){

    stop("You first need to compute Integrated Gradients values using 'sensitivity_analysis()'!")

  }

  ig_tables <- tables[ig_names]

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Integrated Gradients Importance Results")

    if (analysis_object$outcome_levels > 2){

      N <- length(ig_names)

      for (i in 1:N){

        cli::cli_h2(sub(".*_", "", ig_names[[i]]))

        print(ig_tables[[i]])

        cli::cat_line()

      }

    } else{

      print(ig_tables)

    }

  }

  invisible(ig_tables)

}

#' Olden Results Table
#'
#' @description
#'
#' The **table_olden_results()** function extracts results from the Olden method,
#' a technique specific to neural networks that calculates relative importance of
#' input variables through analysis of connection weights between network layers.
#' This method provides a measure of each variable's contribution based on the
#' magnitude and direction of synaptic connections.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Olden")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with Olden results.
#' @examples
#' # Note: For obtaining the table with Olden method results the user needs to
#' # complete till sensitivity_analysis() function of the MLwrap pipeline using
#' # the Olden method. Remember Olden method only can be used with neural
#' # network model.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis
#' # (Requires sensitivity_analysis(methods = "Olden"))
#' # Final call signature:
#' # table_olden_results(wrap_object)
#'
#' @export
table_olden_results <- function(analysis_object, show_table = FALSE){

  olden <- analysis_object$tables$Olden

  if (is.null(olden)){

    stop("You first need to compute Olden values using 'sensitivity_analysis()'!")

  }

  if (analysis_object$outcome_levels < 2){

    olden <- olden[order(-abs(olden$Importance)), ]

  }

  if (base::interactive() && show_table){

    cli::cat_line()

    cli::cli_h1("Olden Importance Results")

    print(olden)

    cli::cat_line()

  }

  invisible(olden)

}

#' Sobol-Jansen Results Table
#'
#' @description
#'
#' The **table_sobol_jansen_results()** function processes results from
#' Sobol-Jansen global sensitivity analysis, a variance decomposition-based
#' methodology that quantifies each variable's contribution and their
#' interactions to the total variability of model predictions. This technique
#' is particularly valuable for identifying higher-order effects and complex
#' interactions between variables.
#'
#' @param analysis_object Fitted analysis_object with 'sensitivity_analysis(methods = "Sobol_Jansen")'.
#'
#' @param show_table Boolean. Whether to print the table.
#'
#' @returns Tibble or list of tibbles (multiclass classification) with Sobol-Jansen results.
#' @examples
#' # Note: For obtaining the table with Sobol_Jansen method results the user
#' # needs to complete till sensitivity_analysis() function of the MLwrap
#' # pipeline using the Sobol_Jansen method. Sobol_Jansen method only works
#' # when all input features are continuous.
#'
#' @seealso \code{\link{sensitivity_analysis}}
#'
#' @examples
#' # See the full pipeline example under sensitivity_analysis
#' # (Requires sensitivity_analysis(methods = "Sobol_Jansen"))
#' # Final call signature:
#' # table_sobol_jansen_results(wrap_object)
#'
#' @export
table_sobol_jansen_results <- function(analysis_object, show_table = FALSE){

  ### Check_args

  sobol <- analysis_object$tables$Sobol_Jansen

  if (is.null(sobol)){

    stop("You first need to compute sobol_jansen values using 'sensitivity_analysis()'!")

  }

  if (base::interactive() && show_table){

    cli::cli_h1("Sobol_Jansen Importance Results")

    print(sobol)

    cli::cat_line()

  }


  invisible(sobol)

}
