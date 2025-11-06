
#' Create ML Model
#'
#' @description
#'
#' The function **build_model()** is designed to construct and attach a ML model
#' to an existing analysis object,which contains the preprocessed dataset
#' generated in the previous step using the preprocessing() function. Based on
#' the specified model type and optional hyperparameters, it supports several
#' popular algorithms—including **Neural Network**, **Random Forest**,
#' **XGBOOST**, and **SVM** (James et al., 2021)— by initializing the
#' corresponding hyperparameter class, updating the analysis object with these
#' settings, and invoking the appropriate model creation function. For SVM
#' models, it further distinguishes between kernel types (rbf, polynomial,
#' linear) to ensure the correct implementation. The function also updates the
#' analysis object with the model name, the fitted model, and the current
#' processing stage before returning the enriched object, thereby streamlining
#' the workflow for subsequent training, evaluation, or prediction steps. This
#' modular approach facilitates flexible and reproducible ML pipelines by
#' encapsulating both the model and its configuration within a single
#' structured object.
#'
#' @param analysis_object analysis_object created from preprocessing function.
#'
#' @param model_name Name of the ML Model. A string of the model name:
#' "Neural Network", "Random Forest", "SVM" or "XGBOOST".
#' @param hyperparameters Hyperparameters of the ML model. List containing
#' the name of the hyperparameter and its value or range of values.
#'
#' @section Hyperparameters:
#'
#' ## Neural Network
#'
#' Parsnip model using **brulee** engine. Hyperparameters:
#'
#' * **hidden_units**: Number of Hidden Neurons.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(5, 20).
#'
#' * **activation**: Activation Function.
#' A vector with any of ("relu", "sigmoid", "tanh") or NULL for default values c("relu", "sigmoid", "tanh").
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, -1) in log10 scale.
#'
#' ## Random Forest
#'
#' Parsnip model using **ranger** engine. Hyperparameters:
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)`. Default range c(100, 300).
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 8).
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(5, 25).
#'
#' ## XGBOOST
#'
#' Parsnip model using **xgboost** engine. Hyperparameters:
#'
#' * **trees**: Number of Trees.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(100, 300).
#'
#' * **mtry**: Number of variables randomly selected as candidates at each split.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 8).
#'
#' * **min_n**: Minimum Number of samples to split at each node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(5, 25).
#'
#' * **tree_depth**: Maximum tree depth.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(3, 8).
#'
#' * **learn_rate**: Learning Rate.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, -1) in log10 scale.
#'
#' * **loss_reduction**: Minimum loss reduction required to make a further partition on a leaf node.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, 1.5) in log10 scale.
#'
#' ## SVM
#'
#' Parsnip model using **kernlab** engine. Hyperparameters:
#'
#' * **cost**: Penalty parameter that regulates model complexity and misclassification tolerance.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-3, 3) in log2 scale.
#'
#' * **margin**: Distance between the separating hyperplane and the nearest data points.
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(0, 0.2).
#'
#' * **type**: Kernel to be used.
#' A single value from ("linear", "rbf", "polynomial"). Default: "linear".
#'
#' * **rbf_sigma**:
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-5, 0) in log10 scale.
#'
#' * **degree**: Polynomial Degree (polynomial kernel only).
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(1, 3).
#'
#' * **scale_factor**: Scaling coefficient applied to inputs. (polynomial kernel only)
#' A single value, a vector with range values `c(min_val, max_val)` or NULL for default range c(-5, -1) in log10 scale.
#'
#' @returns An updated analysis_object containing the fitted machine learning
#' model, the model name, the specified hyperparameters, and the current
#' processing stage. This enriched object retains all previously stored
#' information from the preprocessing step and incorporates the results of the
#' model-building process, ensuring a coherent and reproducible workflow for
#' subsequent training, evaluation, or prediction tasks.
#' @examples
#' # Example 1: Random Forest for regression task
#'
#' library(MLwrap)
#'
#' data(sim_data) # sim_data is a simulated dataset with psychological variables
#'
#' wrap_object <- preprocessing(
#'      df = sim_data,
#'      formula = psych_well ~ depression + emot_intel + resilience + life_sat,
#'      task = "regression"
#'      )
#'
#' wrap_object <- build_model(
#'                analysis_object = wrap_object,
#'                model_name = "Random Forest",
#'                hyperparameters = list(
#'                                  mtry = 2,
#'                                  trees = 10
#'                                  )
#'                            )
#' # It is safe to reuse the same object name (e.g., wrap_object, or whatever)
#' # step by step, as all previous results and information are retained within
#' # the updated analysis object.
#'
#' # Example 2: SVM for classification task
#'
#' data(sim_data) # sim_data is a simulated dataset with psychological variables
#'
#' wrap_object <- preprocessing(
#'          df = sim_data,
#'          formula = psych_well_bin ~ depression + emot_intel + resilience + life_sat,
#'          task = "classification"
#'          )
#'
#' wrap_object <- build_model(
#'                analysis_object = wrap_object,
#'                model_name = "SVM",
#'                hyperparameters = list(
#'                                  type = "rbf",
#'                                  cost = 1,
#'                                  margin = 0.1,
#'                                  rbf_sigma = 0.05
#'                                  )
#'                            )
#' @references
#' James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An Introduction
#' to Statistical Learning: with Applications in R (2nd ed.)*. Springer.
#' https://doi.org/10.1007/978-1-0716-1418-1
#' @export

build_model <- function(analysis_object, model_name, hyperparameters = NULL){

  check_args_build_model(analysis_object = analysis_object, model_name = model_name,
                         hyperparameters = hyperparameters)

  analysis_object = analysis_object$clone()

  task = analysis_object$task

  if (model_name == "Neural Network"){

    if (!requireNamespace("torch", quietly = TRUE)) {
      message("The 'torch' package is not installed. Please run:\n  install.packages('torch')\n  torch::install_torch()")
    }

    if (!requireNamespace("brulee", quietly = TRUE)) {
      message("The 'brulee' package is not installed. Please run:\n install.packages('brulee')\n")

    }

    hyperparams_nn = HyperparamsNN$new(hyperparameters)

    analysis_object$modify("hyperparameters", hyperparams_nn)

    model = create_nn(hyperparams = hyperparams_nn, task = task, epochs = 25)

  } else if (model_name == "XGBOOST"){

    if (!base::requireNamespace("xgboost", quietly = TRUE)) {
      message("The 'xgboost' package is not installed. Please run:\n install.packages('xgboost')\n")
    }

    hyperparams_xgboost = HyperparamsXGBoost$new(hyperparameters)

    hyperparams_xgboost <- check_mtry(analysis_object, hyperparams_xgboost)

    analysis_object$modify("hyperparameters", hyperparams_xgboost)

    model = create_xgboost(hyperparams = hyperparams_xgboost, task = task)

  } else if (model_name == "Random Forest"){

    if (!requireNamespace("ranger", quietly = TRUE)) {
      message("The 'ranger' package is not installed. Please run:\n install.packages('ranger')\n")

    }

    hyperparams_rf = HyperparamsRF$new(hyperparameters)

    hyperparams_rf <- check_mtry(analysis_object, hyperparams_rf)

    analysis_object$modify("hyperparameters", hyperparams_rf)

    model = create_rf(hyperparams = hyperparams_rf, task = task)


  } else if (model_name == "SVM"){

    if (!requireNamespace("kernlab", quietly = TRUE)) {
      message("The 'kernlab' package is not installed. Please run:\n install.packages('kernlab')\n")

    }

    if (is.null(hyperparameters$type)){hyperparameters$type = "linear"}

    type = hyperparameters$type

    hyperparams_svm = HyperparamsSVM$new(hyperparameters)

    # margin only available for regression

    if (task == "classification"){

      hyperparams_svm$margin_tune <- FALSE

      hyperparams_svm$hyperparams_ranges$margin <- NULL

      hyperparams_svm$hyperparams_constant$margin <- NULL

      if (length(hyperparams_svm$hyperparams_ranges) == 0){

        hyperparams_svm$tuning <- FALSE

      }

    }

    analysis_object$modify("hyperparameters", hyperparams_svm)

    if (type == "rbf"){

      model = create_svm_rbf(hyperparams = hyperparams_svm, task = task)

    } else if (type == "poly"){

      model = create_svm_poly(hyperparams = hyperparams_svm, task = task)

    } else if (type == "linear"){

      model = create_svm_linear(hyperparams = hyperparams_svm, task = task)

    }

  } else {

    stop("
    Unrecognized Model. Select from: 'Neural Network', 'XGBOOST', 'Random Forest',
    'SVM'.
          ")

  }

  analysis_object$modify("model_name", model_name)

  analysis_object$modify("model", model)

  analysis_object$modify("stage", "build_model")

  return(analysis_object)
}
