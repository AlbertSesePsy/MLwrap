create_workflow <- function(analysis_object){

  workflow = workflows::workflow() %>%
    workflows::add_recipe(analysis_object$transformer) %>%
    workflows::add_model(analysis_object$model)

  return(workflow)
}

split_data <- function(analysis_object, prop_train = 0.6, prop_val = 0.2){

  model_name = analysis_object$model_name

  y = all.vars(analysis_object$formula)[1]

  tuner = analysis_object$tuner

    if (analysis_object$task == "classification"){

    train_test_split = rsample::initial_split(analysis_object$full_data, prop = 0.75,
                                              strata = !!y)

    }

    else {

      train_test_split = rsample::initial_split(analysis_object$full_data, prop = 0.75)

    }

    analysis_object$modify("train_data", rsample::training(train_test_split))
    analysis_object$modify("test_data", rsample::testing(train_test_split))

    train_data_id <- train_test_split$in_id
    test_data_id <- setdiff(1:nrow(analysis_object$full_data), train_data_id)

    data_id = list(train_data_id = train_data_id, test_data_id = test_data_id)

    analysis_object$modify("data_id", data_id)

    sampling_method <- rsample::vfold_cv(analysis_object$train_data, v = 5)

    final_split <- analysis_object$train_data

  return(list(sampling_method = sampling_method, final_split = final_split))

}


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

  rec =  analysis_object$transformer %>%
    recipes::prep(training = analysis_object$full_data)

  bake_train = recipes::bake(rec, new_data = analysis_object$full_data)

  n_features = ncol(bake_train) - 1

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
