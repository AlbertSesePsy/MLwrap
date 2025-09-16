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

  if (tuner == "Bayesian Optimization"){

    if (analysis_object$task == "classification"){

          validation_split = rsample::initial_validation_split(analysis_object$full_data,
                                                       strata = !!y,
                                                       prop = c(prop_train, prop_val))
    } else {

      validation_split = rsample::initial_validation_split(analysis_object$full_data,
                                                           prop = c(prop_train, prop_val))

    }

  analysis_object$modify("train_data", rsample::training(validation_split))
  analysis_object$modify("validation_data", rsample::validation(validation_split))
  analysis_object$modify("test_data", rsample::testing(validation_split))

  sampling_method <- rsample::validation_set(validation_split)

  final_split = rbind(analysis_object$train_data, analysis_object$validation_data)

  }

  else{

    if (analysis_object$task == "classification"){

    train_test_split = rsample::initial_split(analysis_object$full_data, prop = 0.75,
                                              strata = !!y)

    }

    else {

      train_test_split = rsample::initial_split(analysis_object$full_data, prop = 0.75)

    }

    analysis_object$modify("train_data", rsample::training(train_test_split))
    analysis_object$modify("test_data", rsample::testing(train_test_split))

    sampling_method <- rsample::vfold_cv(analysis_object$train_data, v = 5)

    final_split <- analysis_object$train_data

  }

  return(list(sampling_method = sampling_method, final_split = final_split))

}


create_metric_set <- function(metrics){

  set_metrics <- yardstick::metric_set(!!!rlang::syms(metrics))



  #set_metrics <- yardstick::metric_set("")

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
