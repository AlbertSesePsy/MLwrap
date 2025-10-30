########################################
#             Predictions              #
########################################

get_predictions_multiclass <- function(analysis_object, new_data = "test"){

  model_workflow <- analysis_object$final_model

  y = all.vars(analysis_object$formula)[1]

  if (new_data == "all"){

      data_sets = c("train", "test")

    temp = list()

    for (data_set in data_sets){

      dat = analysis_object[[paste0(data_set, "_data")]]

      predictions_class = predict(model_workflow, new_data = dat)
      predictions_prob = predict(model_workflow, new_data = dat, type = "prob")
      predictions = cbind(predictions_class, predictions_prob, y = as.factor(dat[[y]]))
      predictions$data_set = data_set

      temp[[data_set]] = predictions
    }

    predictions = rbind(temp[["train"]], temp[["test"]])

  } else {

    dat = analysis_object[[paste0(new_data, "_data")]]

    predictions_class = predict(model_workflow, new_data = dat)
    predictions_prob = predict(model_workflow, new_data = dat, type = "prob")
    predictions = cbind(predictions_class, predictions_prob, y = as.factor(dat[[y]]))
    predictions$data_set = new_data

  }

  return (predictions)

}

########################################
#             SUMMARY                  #
########################################

binarize_class <- function(data, target_class) {
  data_bin <- data
  data_bin$truth <- factor(ifelse(data$y == target_class, 1, 0), levels = c(0,1))
  data_bin$prob_estimate <- data[[paste0(".pred_", target_class)]]
  data_bin$estimate <- factor(ifelse(data$.pred_class == target_class, 1, 0), levels = c(0,1))
  data_bin <- data_bin[, c("truth", "estimate", "prob_estimate")]
  return(data_bin)

}

summary_multiclass_per_class <- function(predictions, new_data = "test"){

  class_metrics_funcs <- list(

    Accuracy = accuracy,
    Balanced_Accuracy = bal_accuracy,
    F1_score = f_meas,
    Recall = recall,
    Precision = precision,
    Specificity = specificity,
    Sensitivity = sensitivity,
    MCC = mcc,
    J_index = j_index,
    Detection_Prevalence = detection_prevalence,
    Kappa = kap

  )

  prob_metrics_funcs <- list(

    AUC_ROC = yardstick::roc_auc,
    AUC_PR = yardstick::pr_auc,
    Gain_Capture = yardstick::gain_capture,
    Brier_Score = yardstick::brier_class

  )

  y_classes = levels(predictions$y)

  prob_pred = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  results = lapply(y_classes, function(target_class){

    data_bin <- binarize_class(predictions, target_class)

    unlist(c(
      lapply(class_metrics_funcs, function(metric_fn){
        metric_fn(data_bin, truth = truth, estimate = estimate)$.estimate
      }),
      lapply(prob_metrics_funcs, function(metric_fn){
        metric_fn(data_bin, truth = truth, prob_estimate, event_level = "second")$.estimate
      })

    ))
  })

  names(results) <- y_classes

  results_df <- do.call(rbind, results)
  results_df <- as.data.frame(results_df)
  results_df["Class"] = y_classes
  results_df <- dplyr::relocate(results_df, "Class", .before = 1)

  rownames(results_df) <- NULL

  results_df <- results_df %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ base::signif(.x, 3)))

  return(results_df)

}

summary_multiclass_average <- function(predictions, new_data = "test"){

  metric_funcs <- list(

    Accuracy = function(data) accuracy(data, y, .pred_class),
    Balanced_Accuracy = function(data) bal_accuracy(data, y, .pred_class),
    Precision = function(data) precision(data, y, .pred_class),
    Recall = function(data) recall(data, y, .pred_class),
    Specificity = function(data) specificity(data, y, .pred_class),
    Sensitivity = function(data) sensitivity(data, y, .pred_class),
    Kappa = function(data) kap(data, y, .pred_class),
    F1_score = function(data) f_meas(data, y, .pred_class),
    MCC = function(data) mcc(data, y, .pred_class),
    J_index = function(data) j_index(data, y, .pred_class),
    Detection_Prevalence = function(data) detection_prevalence(data, y, .pred_class, event_level = "second"),
    AUC_ROC = function(data) yardstick::roc_auc(data, y, prob_pred, event_level = "second"),
    AUC_PR = function(data) yardstick::pr_auc(data, y, prob_pred, event_level = "second"),
    Gain_Capture = function(data) yardstick::gain_capture(data, y, prob_pred, event_level = "second"),
    Brier_Score = function(data) yardstick::brier_class(data, y, prob_pred, event_level = "second")

  )

  y_classes = levels(predictions$y)

  prob_pred = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  results <- lapply(metric_funcs, function(f) f(predictions)$.estimate)

  results <- as.data.frame(results)

  return(results)

}

########################################
#             Plots                  #
########################################

plot_roc_curve_multiclass <- function(predictions, new_data = "all"){

  y_classes = levels(predictions$y)

  predicted = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::roc_curve(y, predicted)

    return(curve_plot)

  }

}

plot_pr_curve_multiclass <- function(predictions, new_data = "all"){

  y_classes = levels(predictions$y)

  predicted = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::pr_curve(y, predicted)

    return(curve_plot)

  }


}

plot_gain_curve_multiclass <- function(predictions, new_data = "all"){

  y_classes = levels(predictions$y)

  predicted = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::gain_curve(y, predicted)

    return(curve_plot)

  }


}

plot_lift_curve_multiclass <- function(predictions, new_data = "all"){

  y_classes = levels(predictions$y)

  predicted = unlist(lapply(y_classes, function(target_class) paste0(".pred_", target_class)))

  if (new_data == "all"){

    curve_plot <- predictions %>%
      dplyr::group_by(data_set) %>%
      yardstick::lift_curve(y, predicted)

    return(curve_plot)

  }
}

plot_dist_probs_multiclass <- function(predictions, data_set = "all"){

  df_long <- predictions %>%
    dplyr::filter(data_set == data_set) %>%
    dplyr::select(-c(.pred_class)) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(".pred_"),
                 names_to = "Class",
                 values_to = "Probability") %>%
    dplyr::mutate(Class = base::sub("^\\.pred_", "Output ", Class))

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Probability, fill = y, color = y)) +
    ggplot2::geom_density(alpha = 0.5, bw = 0.1) +
    ggplot2::facet_wrap(~Class) +  # Facet por clase verdadera
    ggplot2::labs(title = paste0("Probability Density for each Class (", data_set, " data)"),
         x = "Output", y = "Density") +
    ggplot2::theme_minimal()

  return(p)

}

