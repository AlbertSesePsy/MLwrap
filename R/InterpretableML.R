###########################
#       Interpretable ML
###########################

#' Perform Sensitivity Analysis and Interpretable ML methods
#'
#' @name sensitivity_analysis
#' @aliases sensitivity_analysis
#'
#' @description
#' As the final step in the MLwrap package workflow, this function performs
#' Sensitivity Analysis (SA) on a fitted ML model stored in an `analysis_object`
#' (in the examples, e.g., tidy_object). It evaluates the importance of
#' features using various methods such as Permutation Feature Importance (PFI),
#' SHAP (SHapley Additive exPlanations), Integrated Gradients, Olden
#' sensitivity analysis, and Sobol indices. The function generates numerical
#' results and visualizations (e.g., bar plots, box plots, beeswarm plots) to
#' help interpret the impact of each feature on the model's predictions for
#' both regression and classification tasks, providing critical insights after
#' model training and evaluation.
#'
#' Following the steps of data preprocessing, model fitting, and performance
#' assessment in the MLwrap pipeline, *sensitivity_analysis()* processes the
#' training and test data using the preprocessing recipe stored in the
#' analysis_object, applies the specified SA methods, and stores the results
#' within the analysis_object. It supports different metrics for evaluation and
#' handles multi-class classification by producing class-specific analyses and
#' plots, ensuring a comprehensive understanding of model behavior
#' (Iooss & Lemaître, 2015).
#'
#' @param analysis_object analysis_object created from fine_tuning function.
#' @param methods Method to be used. A string of the method name: "PFI"
#'     (Permutation Feature Importance), "SHAP" (SHapley Additive exPlanations),
#'     "Integrated Gradients" (Neural Network only), "Olden" (Neural Networks
#'     only), "Sobol_Jansen" (only when all input features are continuous),
#'     "Friedman H-stat" (Friedman's H-statistics for feature interaction).
#' @param use_test Logical. Compute methods using the test set instead of the training set (default = FALSE).
#' @param  metric Metric used for "PFI" method (Permutation Feature Importance).
#'     A string of the name of metric (see Metrics).
#' @details
#' As the concluding phase of the MLwrap workflow—after data preparation,
#' model training, and evaluation—this function interprets models by
#' quantifying and visualizing feature importance. It validates input with
#' `check_args_sensitivity_analysis()`, preprocesses data using the recipe
#' stored in `analysis_object$transformer`, then calculates feature importance
#' via the specified `methods`:
#' - **PFI (Permutation Feature Importance):** Assesses importance by shuffling
#'     feature values and measuring the change in model performance (using the
#'     specified or default `metric`).
#' - **SHAP (SHapley Additive exPlanations):** Computes SHAP values to explain
#'     individual predictions by attributing contributions to each feature.
#' - **Integrated Gradients:** Evaluates feature importance by integrating
#'     gradients of the model's output with respect to input features.
#' - **Olden:** Calculates sensitivity based on connection weights, typically
#'     for neural network models, to determine feature contributions.
#' - **Sobol_Jansen:** Variance-based global sensitivity analysis that
#'     decomposes model output variance into contributions from individual
#'     features and their interactions. Quantifies how much each feature
#'     accounts for prediction variability. Only for continuous outcomes.
#'     Estimates first-order and total-order Sobol indices using the Jansen
#'     (1999) Monte Carlo estimator.
#' - **Friedman H-stat:** Computes the Friedman H-statistic for **each feature**.
#'     It measures the strength of interaction effects relative to main effects,
#'     following the formulation in *Interpretable Machine Learning* (Christoph
#'     Molnar). After ranking features by global H-statistic, the top 5 features
#'     are selected and **all their pairwise interactions** are computed,
#'     returning both **raw interaction strength** and
#'     **normalized interaction scores** (0–1).
#'
#' For classification tasks with more than two outcome levels, the function
#' generates separate results and plots for each class. Visualizations include
#' bar plots for importance metrics, box plots for distribution of values, and
#' beeswarm plots for detailed feature impact across observations. All results
#' are stored in the `analysis_object` under the `sensitivity_analysis` slot,
#' finalizing the MLwrap pipeline with a deep understanding of model drivers.
#' @returns An updated \code{analysis_object} containing sensitivity
#' analysis results. Results are stored in the
#' \code{sensitivity_analysis} slot as a list, with each method's
#' results accessible by name. Generates bar, box, and beeswarm
#' plots for feature importance visualization, completing the
#' workflow with actionable insights.
#' @examples
#' # Example: Using PFI
#'
#' wrap_object <- preprocessing(
#'        df = sim_data,
#'        formula = psych_well ~ depression + life_sat,
#'        task = "regression"
#'        )
#' wrap_object <- build_model(
#'                analysis_object = wrap_object,
#'                model_name = "Random Forest",
#'                hyperparameters = list(
#'                                  mtry = 2,
#'                                  trees = 3
#'                                  )
#'                            )
#' set.seed(123) # For reproducibility
#' wrap_object <- fine_tuning(wrap_object,
#'                 tuner = "Grid Search CV",
#'                 metrics = c("rmse")
#'                 )
#' wrap_object <- sensitivity_analysis(wrap_object, methods = "PFI")
#'
#' # Extracting Results
#'
#' table_pfi <- table_pfi_results(wrap_object)
#'
#' @references
#' Iooss, B., & Lemaître, P. (2015). A review on global sensitivity
#' analysis methods. In: G. Dellino & C. Meloni (Eds.),
#' \emph{Uncertainty Management in Simulation-Optimization of
#' Complex Systems. Operations Research/Computer Science Interfaces
#' Series} (vol. 59). Springer, Boston, MA.
#' \doi{10.1007/978-1-4899-7547-8_5}
#'
#' Jansen, M. J. W. (1999). Analysis of variance designs for model output.
#' *Computer Physics Communications, 117*(1-2), 35–43.
#' \doi{10.1016/S0010-4655(98)00154-4}
#' 
#' Molnar, C. (2022). *Interpretable Machine Learning*.\cr
#' \url{https://christophm.github.io/interpretable-ml-book/}
#' @export
sensitivity_analysis <- function(analysis_object, methods = c("PFI"), metric = NULL, use_test = FALSE){

  check_args_sensitivity_analysis(analysis_object = analysis_object, methods = methods, metric = metric)

  analysis_object = analysis_object$clone()

  task = analysis_object$task

  y = analysis_object$dep_var

  if (task == "classification"){

    y_classes = levels(analysis_object$data$transformed$train_data[[y]])

  }

  if (use_test){

    data <- analysis_object$data$transformed$test_data

    data_label <- " (Test Data)"

  } else {

    data <- analysis_object$data$transformed$train_data

    data_label <- " (Train Data)"

  }

  model_parsnip <- analysis_object$final_model %>%
    tune::extract_fit_parsnip()

  if (is.null(analysis_object$sensitivity_analysis)){

    sensitivity_analysis_list = list()

  } else {

    sensitivity_analysis_list = analysis_object$sensitivity_analysis

  }

  feature_names <- analysis_object$feature_names

  plot_ob = analysis_object$plots

  table_ob = analysis_object$tables

  if ("PFI" %in% methods){

    method_name = "PFI"

    if (is.null(metric)){

      if (task == "regression"){

        metric = "rmse"

      } else{

        metric = "bal_accuracy"

      }
    }

    results <- pfi_calc(model = model_parsnip, data = data, y = y,
                        task = task, metric = metric, outcome_levels = analysis_object$outcome_levels)

    sensitivity_analysis_list[["PFI"]] <- results

    if (analysis_object$outcome_levels > 2){

      final_table <- do.call(
        rbind,
        lapply(y_classes, function(cls) {
          df <- results[[cls]]
          df$output_class <- cls
          df
        })
      )

      table_ob[["PFI"]] <- final_table

      p <- plot_multi_pfi(final_table)

      p$labels$title <- paste0(p$labels$title, data_label)

      plot_ob[["PFI_barplot"]] <- p

    } else{

      p <- plot_barplot(results, func = NULL, title = "Permutation Feature Importance", x_label = "Importance")

      p$labels$title <- paste0(p$labels$title, data_label)

      plot_name <- paste0(method_name,"_barplot")

      plot_ob[[plot_name]] = p

      table_ob[["PFI"]] <- results

    }

  }

  if ("SHAP" %in% methods){

    method_name = "SHAP"

    results <- shap_calc(model = model_parsnip, train = analysis_object$data$transformed$train_data, test = analysis_object$data$transformed$test_data, y = y,
                         task = task, outcome_levels = analysis_object$outcome_levels, use_test = use_test)

    sensitivity_analysis_list[["SHAP"]] <- results

    X_orig <- data[analysis_object$feature_names]

    if (analysis_object$outcome_levels > 2){

      final_table <- do.call(
        rbind,
        lapply(y_classes, function(cls) {
          df <- summarize_importance(
            results[[cls]],
            X_orig,
            feature_names
          )
          df$output_class <- cls
          df
        })
      )

      table_ob[["SHAP"]] <- final_table

      p1 <- plot_multi_abs(final_table, "Mean |SHAP|", "Mean |SHAP| Barplot")

      p1$labels$title <- paste0(p1$labels$title, data_label)

      plot_ob[["SHAP_barplot"]]  <- p1

      p2 <- plot_multi_directional(final_table, "SHAP Directional Plot")

      p2$labels$title <- paste0(p2$labels$title, data_label)

      plot_ob[["SHAP_directional"]] <- p2

      shap_long <- build_importance_long(
        results = results,
        X_orig = X_orig,
        y_classes = y_classes
      )

      plot_ob[["SHAP_boxplot"]] <- plot_boxplot_multi(shap_long, "SHAP value", paste0("SHAP Boxplot", data_label))

      plot_ob[["SHAP_swarmplot"]] <- plot_beeswarm_multi(shap_long, "SHAP value", paste0("SHAP Beeswarm Plot", data_label))

    } else{

    p <- plot_barplot(results, func = function(x) mean(abs(x)),
                 func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                 x_label = "Mean |SHAP|",
                 title = paste0("Mean |SHAP| value", data_label))

    plot_name <- paste0(method_name, "_barplot")

    plot_ob[[plot_name]] = p

    p <- plot2(results, X_orig, func = function(x) mean(x),
            func_se = function(x) sd(x),
            x_label = "Mean (SHAP * sign(X))",
            title = paste0("Directional SHAP Values", data_label))

    plot_name <- paste0(method_name, "_directional")

    plot_ob[[plot_name]] = p

    p <- plot_boxplot(results, y_label = "SHAP value", title = paste0("SHAP Value Distribution", data_label))
    plot_name <- paste0(method_name, "_boxplot")

    plot_ob[[plot_name]] = p

    p <- plot_beeswarm(results, X_orig = X_orig, x_label = "SHAP value", title = paste0("SHAP Swarm Plot", data_label))

    plot_name <- paste0(method_name, "_swarmplot")

    plot_ob[[plot_name]] = p

    table_name <- paste0(method_name)

    table_ob[[table_name]] <- summarize_importance(results, X_orig, feature_names)

    }

  }

  if ("Integrated Gradients" %in% methods){

    method_name = "IntegratedGradients"

    results <- IntGrad_calc(model = model_parsnip, train = analysis_object$data$transformed$train_data, test = analysis_object$data$transformed$test_data, y = y,
                            task = task, outcome_levels = analysis_object$outcome_levels, use_test = use_test)

    sensitivity_analysis_list[["IntegratedGradients"]] <- results

    X_orig <- data[analysis_object$feature_names]

    if (analysis_object$outcome_levels > 2){

      final_table <- do.call(
        rbind,
        lapply(y_classes, function(cls) {
          df <- summarize_importance(
            results[[cls]],
            X_orig,
            feature_names
          )
          df$output_class <- cls
          df
        })
      )

      table_ob[["IntegratedGradients"]] <- final_table

      plot_ob[["IntegratedGradients_barplot"]] <-
        plot_multi_abs(final_table,
                       y_label = "Mean |Integrated Gradients|",
                       title = paste0("Mean |Integrated Gradients|", data_label))

      plot_ob[["IntegratedGradients_directional"]] <-
        plot_multi_directional(final_table,
                               title = paste0("Directional Integrated Gradients", data_label))

      ig_long <- build_importance_long(
        results = results,
        X_orig = X_orig,
        y_classes = y_classes
      )

      plot_ob[["IntegratedGradients_boxplot"]] <-
        plot_boxplot_multi(ig_long, y_label = "Integrated Gradient", title = paste0("Integrated Gradient Boxplot", data_label))

      plot_ob[["IntegratedGradients_swarmplot"]] <-
        plot_beeswarm_multi(ig_long, x_label = "Integrated Gradient", title = paste0("Integrated Gradient Beeswarm", data_label))

    } else{

      p <- plot_barplot(results, func = function(x) mean(abs(x)),
                   func_se = function(x) sd(abs(x)) / sqrt(length(x)),
                   x_label = "Mean |Integrated Gradient|",
                   title = paste0("Mean |Integrated Gradients|", data_label)
                   )

      plot_name <- paste0(method_name,"_barplot")

      plot_ob[[plot_name]] = p

      p <- plot2(results, X_orig, func = function(x) mean(x),
            func_se = function(x) sd(x),
            x_label = "Integradient Gradient Correlation",
            title = paste0("Directional Sensitivity of Integrated Gradients", data_label))

      plot_name <- paste0(method_name,"_directional")

      plot_ob[[plot_name]] = p

      p <- plot_boxplot(results, y_label = "Integrated Gradient value", title = paste0("Integrated Gradients Distribution", data_label))

      plot_name <- paste0(method_name,"_boxplot")

      plot_ob[[plot_name]] = p

      p <- plot_beeswarm(results, X_orig = X_orig, x_label = "Integrated Gradient value",
                    title = paste0("Integrated Gradients Swarm Plot", data_label))

      plot_name <- paste0(method_name,"_swarmplot")

      plot_ob[[plot_name]] = p

      table_name <- paste0(method_name)

      table_ob[[table_name]] <- summarize_importance(results, X_orig, feature_names)

    }

  }

  if ("Olden" %in% methods){

    method_name = "Olden"

    results = olden_calc(model = model_parsnip, task,
                         outcome_levels = analysis_object$outcome_levels, y_classes = y_classes)

    df_results <- as.data.frame(t(results))

    colnames(df_results) <- feature_names

    df_results <- tibble::as_tibble(df_results)

    sensitivity_analysis_list[["Olden"]] <- df_results

    if (analysis_object$outcome_levels > 2) {

      # ---------- 1. Build wide Olden table (Feature × Class) ----------
      table_olden <- df_results %>%
        dplyr::mutate(iter = dplyr::row_number()) %>%
        tidyr::pivot_longer(
          -iter,
          names_to = "Feature",
          values_to = "Importance"
        ) %>%
        dplyr::mutate(
          iter = paste0("Importance (", y_classes[iter], ")")
        ) %>%
        tidyr::pivot_wider(
          names_from = "iter",
          values_from = "Importance"
        )

      table_ob[["Olden"]] <- table_olden

      # ---------- 2. Convert to long format for faceting ----------
      olden_long <- table_olden %>%
        tidyr::pivot_longer(
          cols = -Feature,
          names_to = "output_class",
          values_to = "Importance"
        ) %>%
        dplyr::mutate(
          output_class = gsub("Importance \\(|\\)", "", output_class)
        )

      # ---------- 3. Plot ----------
      plot_ob[["Olden"]] <- plot_olden_multi(olden_long)

    } else{

      p <- olden_barplot(results, feature_names)

      plot_name <- paste0(method_name,"_barplot")

      plot_ob[[plot_name]] = p

      table_olden <- df_results %>%
        dplyr::mutate(iter = dplyr::row_number()) %>%
        tidyr::pivot_longer(-iter, names_to = "Feature", values_to = "Importance") %>%
        dplyr::mutate(iter = paste0("Importance")) %>%
        tidyr::pivot_wider(names_from = iter, values_from = Importance)

      table_ob[["Olden"]] <- table_olden

    }
  }

    if ("Sobol_Jansen" %in% methods){

      sobol <- sobol_calc(model_parsnip, analysis_object$data$transformed$train_data, task, feature_names)

      sensitivity_analysis_list[["Sobol_Jansen"]] <- sobol
      p <- sobol_plot(sobol)

      plot_ob[["Sobol_Jansen"]] = p

      results_table <- tibble::tibble(

        "Feature" = base::rownames(sobol$S),
        "First Order (S)" = sobol$S$original,
        "S StErr" = sobol$S$`std. error`,
        "S Min CI" = sobol$S$`min. c.i.`,
        "S Max CI" = sobol$S$`max. c.i.`,
        "Total Order (T)" = sobol$T$original,
        "T StErr" = sobol$T$`std. error`,
        "T Min CI" = sobol$T$`min. c.i.`,
        "T Max CI" = sobol$T$`max. c.i.`

      )

      table_ob[["Sobol_Jansen"]] <- results_table


    }

  if ("Friedman H-stat" %in% methods){


    h2_tables <- calc_hstats(analysis_object, use_test)

    table_ob[["H^2 Total"]] <- h2_tables$h2_total
    table_ob[["H^2 Pairwise Normalized"]] <- h2_tables$h2_pairwise_norm
    table_ob[["H^2 Pairwise Raw"]] <- h2_tables$h2_pairwise_raw

    # plot

    p1 <- hstat_total_plot(h2_tables$h2_total, outcome_levels = analysis_object$outcome_levels)

    p1$labels$title <- paste0(p1$labels$title, data_label)

    plot_ob[["H^2 Total"]] <- p1

    p2 <- hstat_pairwise_plot(h2_tables$h2_pairwise_norm, outcome_levels = analysis_object$outcome_levels)

    p2$labels$title <- paste0(p2$labels$title, data_label)

    plot_ob[["H^2 Pairwise Normalized"]] <- p2

    p3 <- hstat_pairwise_plot(h2_tables$h2_pairwise_raw,
                              outcome_levels = analysis_object$outcome_levels,
                              normalized = FALSE)

    p3$labels$title <- paste0(p3$labels$title, data_label)

    plot_ob[["H^2 Pairwise Raw"]] <- p3

  }


  analysis_object$modify("sensitivity_analysis", sensitivity_analysis_list)

  analysis_object$modify("plots", plot_ob)

  analysis_object$modify("tables", table_ob)

  return(analysis_object)

}

####################
#   Global Plots   #
####################

#### plot_global

plot_barplot <- function(X, func = NULL, func_se = stats::sd, title, x_label) {

  X <- base::as.data.frame(X)

  if (!is.null(func)){

  summary_df <- tibble::tibble(
      Feature = base::colnames(X),
      Importance = base::sapply(X, func),
      StDev = base::sapply(X, func_se)
    )

  } else{summary_df <- X}



  summary_df$Feature <- factor(summary_df$Feature,
                                levels = summary_df$Feature[order(summary_df$Importance, decreasing = F)])

    p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = Importance, y = Feature)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = Importance - StDev, xmax = Importance + StDev), width = 0.2) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(Importance, 3), " (", round(StDev, 3), ")"),
                                      x = Importance +  max(abs(StDev)) * 0.2  # slight offset
                                      ),
                vjust =  -0.25,
                hjust = -0.2) +
      ggplot2::labs(
        x = x_label,
        y = "Feature",
        title = title
        ) +
      ggplot2::theme_grey() +
      ggplot2::expand_limits(x = max(summary_df$Importance) * 1.5)

  return(p)

}


plot2 <- function(X, test, func = NULL, func_se = stats::sd, title, x_label) {

  X <- base::as.data.frame(X)

  test <- base::as.data.frame(test)

  N = base::ncol(test)

  feat_names <- names(test)

  collapse_imp <- list()

  for (i in 1:N){

    collapse_imp[[feat_names[i]]] <- stats::cov(X[[i]], test[[i]]) / stats::var(test[[i]])

  }

  collapse_imp <- base::as.data.frame(collapse_imp)


   df <- tibble::tibble(
    variable = base::colnames(collapse_imp),
    importance = base::unlist(collapse_imp, use.names = FALSE)
  )

  # Order decreasing
  df$variable <- factor(df$variable, levels = df$variable[order(df$importance, decreasing = T)])

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = importance, fill = importance > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = round(importance, 3)),
                       vjust = ifelse(df$importance >= 0, -0.5, 1.2)) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = title,
      x = "Feature",
      y = "Feature Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::coord_cartesian(clip="off")

  return(p)

}

plot_boxplot <- function(X, title, y_label){

  X <- base::as.data.frame(X)

  summary_df <- tibble::tibble(
    variable = base::colnames(X),
    value = base::sapply(X, function(x) mean(abs(x)))
  )

    X_long <- tidyr::pivot_longer(
      data = X,
      cols = tidyr::everything(),
      names_to = "variable",
      values_to = "value"
    )

    X_long$variable <- factor(X_long$variable,
                                  levels = summary_df$variable[order(summary_df$value, decreasing = T)])

    p <- ggplot2::ggplot(X_long, ggplot2::aes(x = variable, y = value)) +
      ggplot2::geom_boxplot(fill = "lightgray") +
      ggplot2::labs(
        x = "Feature",
        y = y_label,
        title = title
      ) +
      ggplot2::theme_grey() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)

}

###### Beeswarm

plot_beeswarm <- function(X_vals, X_orig, title, x_label, color_quantiles = c(0.02, 0.98)) {

  order_df <- tibble::tibble(
    variable = base::colnames(X_vals),
    value = base::sapply(X_vals, function(x) mean(abs(x)))
  )

  summary_df <- X_vals |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value")

  summary_df$variable <- factor(summary_df$variable,
                                levels = order_df$variable[order(order_df$value, decreasing = FALSE)])

  X <- X_orig |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "val_color")

  X$variable <- factor(X$variable,
                       levels = order_df$variable[order(order_df$value, decreasing = FALSE)])

  summary_df[["val_color"]] <- X[["val_color"]]

  # 5th / 95th percentiles for the color scale
  q <- stats::quantile(summary_df$val_color, probs = color_quantiles, na.rm = TRUE, names = FALSE)
  if (diff(q) == 0) q <- q + c(-1e-8, 1e-8)  # guard for constant color

  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = value, y = variable, color = val_color)) +
    ggbeeswarm::geom_quasirandom(bandwidth = 0.2, method = "pseudorandom", cex = 2, orientation = "x") +
    ggplot2::labs(x = x_label, y = "Feature", title = title) +
    ggplot2::theme_grey() +
    ggplot2::scale_color_viridis_c(
      option = "A",
      limits = q,                 # clamp to 5th/95th pct
      oob = scales::squish        # keep points, squash extremes to ends
    )

  return(p)
}

###########################
#   Prediction Wrappers   #
###########################

pred_reg <- function(object, newdata){

  return(predict(object,new_data= newdata)$.pred)

}

pred_bin <- function(object, newdata){

  return(predict(object, new_data = newdata, type = "prob")[,2][[1]])

}

pred_bin_class <- function(object, newdata){

  return(predict(object, new_data = newdata, type = "class")$.pred_class)

}

summarize_importance <- function(importance_matrix, original_df,  feature_names) {

  # Calcular media y error estandar para cada variable
  mean_importance <- colMeans(abs(importance_matrix), na.rm = TRUE)
  std_error <- apply(abs(importance_matrix), 2, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
  directional_list = c()

  for (i in 1:length(feature_names)){

    name = feature_names[i]

    if (base::is.factor(original_df[name])){

      m1 <- base::mean(importance_matrix[original_df[name] == 1], na.rm = TRUE)
      m0 <- base::mean(importance_matrix[original_df[name] == 0], na.rm = TRUE)

      directional_list[i] <- m1 - m0

    } else {


    directional_list[i] <- stats::cov(importance_matrix[name], original_df[name]) / stats::var(original_df[name])

    }
  }

  # Crear resumen ordenado
  summary_df <- tibble::tibble(
    "Feature" = feature_names,
    "Mean_Abs_Importance" = mean_importance,
    "StDev" = std_error,
    "Directional_Importance" = directional_list
  )

  # Ordenar de mayor a menor importancia

  summary_df <- summary_df[order(-summary_df[["Mean_Abs_Importance"]]), ]

  return(summary_df)
}


