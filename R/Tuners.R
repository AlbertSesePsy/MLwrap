create_workflow <- function(analysis_object){

  workflow = workflows::workflow() %>%
    workflows::add_recipe(analysis_object$transformer) %>%
    workflows::add_model(analysis_object$model)

  return(workflow)
}
metrics_info <- list(

  #################
  #   Regression
  #################

  rmse = c("numeric", "minimize"),
  mae = c("numeric", "minimize"),
  mpe = c("numeric", "minimize"),
  mape = c("numeric", "minimize"),
  ccc = c("numeric", "maximize"),
  smape = c("numeric", "minimize"),
  rpiq = c("numeric", "maximize"),
  rsq = c("numeric", "maximize"),

  #################
  #  Classification
  #################

  accuracy = c("class", "maximize"),
  precision = c("class", "maximize"),
  recall = c("class", "maximize"),
  bal_accuracy = c("class", "maximize"),
  specificity = c("class", "maximize"),
  sensitivity = c("class", "maximize"),
  kap = c("class", "maximize"),
  f_meas = c("class", "maximize"),
  mcc = c("class", "maximize"),
  j_index = c("class", "maximize"),
  detection_prevalence = c("class", "maximize"),

  roc_auc = c("prob", "maximize"),
  pr_auc = c("prob", "maximize"),
  gain_capture = c("prob", "maximize"),
  brier_class = c("prob", "minimize"),
  roc_aunp = c("prob", "maximize")
)

create_metric_set <- function(metrics){
  set_metrics <- yardstick::metric_set(!!!rlang::syms(metrics))
  return(set_metrics)
  }

extract_hyperparams <- function(analysis_object){

  extracted_hyperparams <-
    analysis_object$workflow %>%
    workflows::extract_parameter_set_dials() %>%
    update(!!!analysis_object$hyperparameters$hyperparams_ranges)

  return(extracted_hyperparams)

}


hyperparams_grid <- function(hyperparams, levels = 10){

  grid = dials::grid_regular(hyperparams$hyperparams_ranges, levels = levels)

  return(grid)

}

tuning_results <- function(analysis_object){

  if (analysis_object$tuner == "Bayesian Optimization"){

    p <- analysis_object$tuner_fit %>%
      tune::autoplot(type = "performance") +
      ggplot2::labs(title = "Bayesian Optimization Iteration Loss")

    plot_ob = analysis_object$plots

    plot_ob$bayesian_opt_iter_loss = p

    analysis_object$modify("plots", plot_ob)

    p <- analysis_object$tuner_fit %>%
      tune::autoplot(., search_res, type = "parameters") +
      ggplot2::labs(x = "Iterations", y = NULL, title = "Bayesian Optimization Iteration Results")

    plot_ob = analysis_object$plots

    plot_ob$bayesian_opt_iter_results = p

    analysis_object$modify("plots", plot_ob)

  }

  p <- analysis_object$tuner_fit %>%
       tune::autoplot() +
       ggplot2::labs(title = paste0(analysis_object$tuner, " Search Results"))

  plot_ob = analysis_object$plots

  plot_ob$tuner_search_results = p

  analysis_object$modify("plots", plot_ob)

  return(analysis_object)

}

check_mtry <- function(analysis_object, hyperparameters){

  analysis_object = analysis_object$clone()

  n_features = length(analysis_object$feature_names)

  if (!is.null(hyperparameters$hyperparams_constant$mtry)){

      if (hyperparameters$hyperparams_constant$mtry > n_features){

        hyperparameters$hyperparams_constant$mtry = n_features

        if (base::interactive()){cli::cli_alert_warning(paste0("'mtry' is greater than total number of features.
                                                     Setting its value to ", n_features, "."))}

      }
  }

  if (!is.null(hyperparameters$hyperparams_ranges$mtry)){

      if (hyperparameters$hyperparams_ranges$mtry$range$upper > n_features){

        hyperparameters$hyperparams_ranges$mtry$range$upper = n_features

        if (base::interactive()){
          cli::cli_alert_warning(paste0(
          "'mtry' upper range is greater than total number of features (",n_features,"). Setting its value to ", n_features, "."))}

      }

      if (hyperparameters$hyperparams_ranges$mtry$range$lower > n_features){

        stop(paste0("Lower range of 'mtry' (",hyperparameters$hyperparams_ranges$mtry$range$lower,") is greater than the number of features: ", n_features, "!"))

      }

  }

  return(hyperparameters)

}
