pfi_plot <- function(tidy_object, new_data = "test", metric = NULL){

  if (tidy_object$task == "regression"){

    pfi_reg(tidy_object, new_data = new_data, metric = metric)

  } else if (tidy_object$task == "classification"){

    pfi_bin(tidy_object, new_data = new_data, metric = metric)

  }

}


                                       ###########################
                                        #     Regression          #
                                        ###########################

pfi_calc <- function(model, train, test, y, task, metric, outcome_levels){

  if (task == "regression"){

    pfi_results <- pfi_reg(model, test, y, metric)

  } else {

    if (outcome_levels == 2){

      pfi_results <- pfi_bin(model, test, y, metric)

    }

    else{

      pfi_results <- pfi_multiclass(model, test, y, metric)

    }

  }

  return(pfi_results)

}

pfi_reg <- function(model, new_data, y, metric){

  # Native Permutation Feature Importance Implementation
  # No external dependencies - uses MLwrap infrastructure

  X <- new_data[, !(names(new_data) %in% y), drop = FALSE]
  y_true <- new_data[[y]]
  feature_names <- names(X)
  n_features <- ncol(X)
  nsim <- 25

  # Baseline predictions
  predictions_baseline <- pred_reg(model, X)

  # Get metric function from MLwrap namespace
  metric_func <- get(metric, envir = asNamespace("MLwrap"))

  # Calculate baseline error using standard evaluation
  df_baseline <- data.frame(truth = y_true, estimate = predictions_baseline)
  error_baseline <- metric_func(df_baseline, truth = "truth", estimate = "estimate")$.estimate

  # Initialize importance matrix
  importance_matrix <- matrix(NA, nrow = n_features, ncol = nsim)
  rownames(importance_matrix) <- feature_names

  # Permutation loop
  for (i in 1:n_features){
    feature_name <- feature_names[i]

    for (sim in 1:nsim){
      X_permuted <- X
      X_permuted[[feature_name]] <- sample(X[[feature_name]], size = nrow(X), replace = FALSE)

      predictions_permuted <- pred_reg(model, X_permuted)
      df_permuted <- data.frame(truth = y_true, estimate = predictions_permuted)
      error_permuted <- metric_func(df_permuted, truth = "truth", estimate = "estimate")$.estimate

      # Importance based on metric direction
      if (metrics_info[[metric]][2] == "minimize"){
        importance_matrix[i, sim] <- error_permuted - error_baseline
      } else {
        importance_matrix[i, sim] <- error_baseline - error_permuted
      }
    }
  }

  importance_mean <- rowMeans(importance_matrix)
  importance_sd <- apply(importance_matrix, 1, sd)

  vis <- data.frame(
    Variable = feature_names,
    Importance = importance_mean,
    StDev = importance_sd,
    stringsAsFactors = FALSE
  )

  vis <- vis[order(-vis$Importance), ]
  rownames(vis) <- NULL
  colnames(vis)[1] <- "Feature"

  return(vis)

}


                                #####################################
                                #     Binary Classification         #
                                #####################################

pfi_bin <- function(model, new_data, y, metric){

  if (metrics_info[[metric]][1] == "prob"){
    pred_func = pred_bin
  } else {
    pred_func = pred_bin_class
  }

  # Native Permutation Feature Importance Implementation
  # No external dependencies - uses MLwrap infrastructure

  X <- new_data[, !(names(new_data) %in% y), drop = FALSE]
  y_true <- new_data[[y]]
  feature_names <- names(X)
  n_features <- ncol(X)
  nsim <- 25

  # Baseline predictions
  predictions_baseline <- pred_func(model, X)

  # Get metric function from MLwrap namespace
  metric_func <- get(metric, envir = asNamespace("MLwrap"))

  # Calculate baseline error using standard evaluation
  if (metrics_info[[metric]][1] == "prob"){
    df_baseline <- data.frame(truth = y_true, .pred_1 = predictions_baseline, check.names = FALSE)
    error_baseline <- metric_func(df_baseline, truth = "truth", ".pred_1", event_level = "second")$.estimate
  } else {
    df_baseline <- data.frame(truth = y_true, estimate = predictions_baseline)
    error_baseline <- metric_func(df_baseline, truth = "truth", estimate = "estimate")$.estimate
  }

  # Initialize importance matrix
  importance_matrix <- matrix(NA, nrow = n_features, ncol = nsim)
  rownames(importance_matrix) <- feature_names

  # Permutation loop
  for (i in 1:n_features){
    feature_name <- feature_names[i]

    for (sim in 1:nsim){
      X_permuted <- X
      X_permuted[[feature_name]] <- sample(X[[feature_name]], size = nrow(X), replace = FALSE)

      predictions_permuted <- pred_func(model, X_permuted)

      if (metrics_info[[metric]][1] == "prob"){
        df_permuted <- data.frame(truth = y_true, .pred_1 = predictions_permuted, check.names = FALSE)
        error_permuted <- metric_func(df_permuted, truth = "truth", ".pred_1", event_level = "second")$.estimate
      } else {
        df_permuted <- data.frame(truth = y_true, estimate = predictions_permuted)
        error_permuted <- metric_func(df_permuted, truth = "truth", estimate = "estimate")$.estimate
      }

      # Importance based on metric direction
      if (metrics_info[[metric]][2] == "minimize"){
        importance_matrix[i, sim] <- error_permuted - error_baseline
      } else {
        importance_matrix[i, sim] <- error_baseline - error_permuted
      }
    }
  }

  importance_mean <- rowMeans(importance_matrix)
  importance_sd <- apply(importance_matrix, 1, sd)

  vis <- data.frame(
    Variable = feature_names,
    Importance = importance_mean,
    StDev = importance_sd,
    stringsAsFactors = FALSE
  )

  vis <- vis[order(-vis$Importance), ]
  rownames(vis) <- NULL
  colnames(vis)[1] <- "Feature"

  return(vis)

}

pfi_multiclass <- function(model, new_data, y, metric){

  y_classes = levels(new_data[[y]])
  new_test <- new_data[, !(names(new_data) %in% y)]
  results = list()

  for (target_class in y_classes){

    new_y <- factor(ifelse(new_data[[y]] == target_class, 1, 0), levels = c(0,1))

    if (metrics_info[[metric]][1] == "prob"){

      predicted = paste0(".pred_", target_class)

      pred_func <- function(object, newdata){

        return(predict(object, new_data = newdata, type = "prob")[[predicted]])

      }

    }

    else{

      pred_func <- function(object, newdata){

        pred = predict(object, new_data = newdata, type = "class")$.pred_class

        bin_pred = factor(ifelse(pred == target_class, 1, 0), levels = c(0,1))

        return(bin_pred)
      }
    }

    # Native Permutation Feature Importance Implementation
    # No external dependencies - uses MLwrap infrastructure

    X <- new_test
    y_true <- new_y
    feature_names <- names(X)
    n_features <- ncol(X)
    nsim <- 25

    # Baseline predictions
    predictions_baseline <- pred_func(model, X)

    # Get metric function from MLwrap namespace
    metric_func <- get(metric, envir = asNamespace("MLwrap"))

    # Calculate baseline error using standard evaluation
    if (metrics_info[[metric]][1] == "prob"){
      df_baseline <- data.frame(truth = y_true, .pred_1 = predictions_baseline, check.names = FALSE)
      error_baseline <- metric_func(df_baseline, truth = "truth", ".pred_1", event_level = "second")$.estimate
    } else {
      df_baseline <- data.frame(truth = y_true, estimate = predictions_baseline)
      error_baseline <- metric_func(df_baseline, truth = "truth", estimate = "estimate")$.estimate
    }

    # Initialize importance matrix
    importance_matrix <- matrix(NA, nrow = n_features, ncol = nsim)
    rownames(importance_matrix) <- feature_names

    # Permutation loop
    for (i in 1:n_features){
      feature_name <- feature_names[i]

      for (sim in 1:nsim){
        X_permuted <- X
        X_permuted[[feature_name]] <- sample(X[[feature_name]], size = nrow(X), replace = FALSE)

        predictions_permuted <- pred_func(model, X_permuted)

        if (metrics_info[[metric]][1] == "prob"){
          df_permuted <- data.frame(truth = y_true, .pred_1 = predictions_permuted, check.names = FALSE)
          error_permuted <- metric_func(df_permuted, truth = "truth", ".pred_1", event_level = "second")$.estimate
        } else {
          df_permuted <- data.frame(truth = y_true, estimate = predictions_permuted)
          error_permuted <- metric_func(df_permuted, truth = "truth", estimate = "estimate")$.estimate
        }

        # Importance based on metric direction
        if (metrics_info[[metric]][2] == "minimize"){
          importance_matrix[i, sim] <- error_permuted - error_baseline
        } else {
          importance_matrix[i, sim] <- error_baseline - error_permuted
        }
      }
    }

    importance_mean <- rowMeans(importance_matrix)
    importance_sd <- apply(importance_matrix, 1, sd)

    vis <- data.frame(
      Variable = feature_names,
      Importance = importance_mean,
      StDev = importance_sd,
      stringsAsFactors = FALSE
    )

    vis <- vis[order(-vis$Importance), ]
    rownames(vis) <- NULL
    colnames(vis)[1] <- "Feature"

    results[[target_class]] <- vis
  }

  return(results)

}
#########################################
#     Multiclass Classification         #
#########################################
