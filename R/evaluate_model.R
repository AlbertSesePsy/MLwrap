# ######################################################
# #         get_results                                #
# ######################################################
#

evaluate_model <- function(analysis_object){

  analysis_object = analysis_object$clone()

  predictions = get_predictions(analysis_object, "all")

  task = analysis_object$task

  analysis_object$modify("predictions", predictions)

  pred_train = predictions %>% dplyr::filter(data_set == "train")

  pred_test = predictions %>% dplyr::filter(data_set == "test")

  summary_train = summary_results(analysis_object, pred_train, new_data = "Train")

  summary_test = summary_results(analysis_object, pred_test, new_data = "Test")

  tables <- analysis_object$tables

  if (analysis_object$outcome_levels > 2){

    tables$summary_train <- summary_train

    tables$summary_test <- summary_test

  } else {

    summary_total <- dplyr::bind_rows(summary_train, summary_test)

    tables$summary_results <- summary_total

  }

  analysis_object$modify("tables", tables)

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
        plot_roc_curve_binary(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "ROC Curve")


    } else {

      p <-  predictions %>%
        plot_roc_curve_multiclass(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "ROC Curve")

    }

    plot_ob = analysis_object$plots

    plot_ob$roc_curve = p

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
        plot_pr_curve_binary(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Precision Recall Curve")


    } else {

      p <- predictions %>%
        plot_pr_curve_multiclass(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Precision Recall Curve")


    }

    plot_ob = analysis_object$plots

    plot_ob$pr_curve = p

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
        plot_gain_curve_binary() %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Gain Curve")


    } else {

      p <-predictions %>%
        plot_gain_curve_multiclass() %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Gain Curve")

    }

    plot_ob = analysis_object$plots

    plot_ob$gain_curve = p

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p <- predictions %>%
        plot_lift_curve_binary(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Lift Curve")


    } else{


      p <- predictions %>%
        plot_lift_curve_multiclass(new_data = "all") %>%
        ggplot2::autoplot() +
        ggplot2::labs(title = "Lift Curve")


    }

    plot_ob = analysis_object$plots

    plot_ob$lift_curve = p

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p_train <- pred_train %>%
        plot_dist_probs_binary("train")

      p_test <- pred_test %>%
        plot_dist_probs_binary("test")

    } else {

      p_train <- pred_train %>%
        plot_dist_probs_multiclass(data_set = "train")

      p_test <- pred_test %>%
        plot_dist_probs_multiclass(data_set = "test")

    }

    plot_ob = analysis_object$plots

    plot_ob$dist_by_class_train = p_train

    plot_ob$dist_by_class_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "classification"){

    if (analysis_object$outcome_levels == 2){

      p_train <- pred_train %>%
        plot_calibration_curve_binary(new_data = "train")

      p_test <- pred_test %>%
        plot_calibration_curve_binary(new_data = "test")

      plot_ob = analysis_object$plots

      plot_ob$reliability_plot_train = p_train

      plot_ob$reliability_plot_test = p_test

      analysis_object$modify("plots", plot_ob)

    }

  }

  if (task == "classification"){

    cm_train <- pred_train %>%
      plot_conf_mat(new_data = "train")

    cm_test <- pred_test %>%
      plot_conf_mat(new_data = "test")

    p_train <- cm_train %>% ggplot2::autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix Train Data")

    p_test <- cm_test %>% ggplot2::autoplot(type = "heatmap") +
      ggplot2::labs(title = "Confusion Matrix Test Data")

    plot_ob = analysis_object$plots

    plot_ob$confusion_matrix_train = p_train

    plot_ob$confusion_matrix_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_scatter(new_data = "train", error = TRUE)

    p_test <- pred_test %>%
      plot_scatter(new_data = "test", error = TRUE)

    plot_ob = analysis_object$plots

    plot_ob$scatter_residuals_train = p_train

    plot_ob$scatter_residuals_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_scatter(new_data = "train", error = F)

    p_test <- pred_test %>%
      plot_scatter(new_data = "test", error = F)

    plot_ob = analysis_object$plots

    plot_ob$scatter_predictions_train = p_train

    plot_ob$scatter_predictions_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  if (task == "regression"){

    p_train <- pred_train %>%
      plot_residuals_density(new_data = "train")

    p_test <- pred_test %>%
      plot_residuals_density(new_data = "test")

    plot_ob = analysis_object$plots

    plot_ob$residuals_dist_train = p_train

    plot_ob$residuals_dist_test = p_test

    analysis_object$modify("plots", plot_ob)

  }

  analysis_object$modify("stage", "evaluated_model")

  return(analysis_object)

}




modify_datasets <- function(analysis_object){

  rec <- recipes::prep(analysis_object$transformer,
                       training = analysis_object$train_data,
                       strings_as_factors = T)

  new_train <- recipes::bake(rec, new_data = analysis_object$train_data)
  new_test <- recipes::bake(rec, new_data = tidy_object$test_data)

  tidy_object$modify("train_data", new_train)
  tidy_object$modify("test_data", new_test)

  if (!is.null(tidy_object$validation_data)){

    new_validation <- recipes::bake(rec, new_data = tidy_object$validation_data)
    tidy_object$modify("validation_data", new_validation)

  }

  print(new_train)

  return(tidy_object)

}

######################################################
#         get_predictions                           #
######################################################

get_predictions <- function(analysis_object, new_data = "test"){

  if (analysis_object$task == "regression"){

    predictions = get_predictions_regression(analysis_object, new_data = new_data)

  } else if (analysis_object$task == "classification"){

    predictions = get_predictions_binary(analysis_object, new_data = new_data)

    if (analysis_object$outcome_levels == 2){

      predictions = get_predictions_binary(analysis_object, new_data = new_data)

    } else {

      predictions = get_predictions_multiclass(analysis_object, new_data = new_data)

    }

  }

  return(predictions)


}

######################################################
#         SUMMARY                                    #
######################################################

summary_results <- function(analysis_object, predictions, new_data = "test"){

  if (analysis_object$task == "regression"){

    return(summary_regression(predictions, new_data))

  } else if (analysis_object$task == "classification"){

    if (analysis_object$outcome_levels == 2){

      return(summary_binary(predictions, new_data))

    } else {

      return(summary_multiclass_per_class(predictions, new_data))

    }

  }

}







