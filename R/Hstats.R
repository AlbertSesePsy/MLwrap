calc_hstats <- function(analysis_object, use_test = FALSE){

  task <- analysis_object$task
  outcome_levels <- analysis_object$outcome_levels

  if (task == "regression"){

    h2_tables <- calc_hstats_regression(analysis_object, use_test)

  } else if (outcome_levels == 2){

    h2_tables <- calc_hstats_binary(analysis_object, use_test)

  } else {

    h2_tables <- calc_hstats_multiclass(analysis_object, use_test)

  }

  return(h2_tables)

}


calc_hstats_regression <- function(analysis_object, use_test){

  if (use_test){

    train_data <- analysis_object$data$raw$test_data %>%
      dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  } else {

  train_data <- analysis_object$data$raw$train_data %>%
    dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  }

  hstats_object <- comp_hstats(analysis_object$final_model,
                               train_data,
                               task = "regression",
                               outcome_levels = analysis_object$outcome_levels)

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5))
  colnames(h2_normalized) <- "H^2 Normalized"

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom

  h2_pairwise_norm <- round(num / denom, 5)
  h2_pairwise_norm <- h2_pairwise_norm[order(-h2_pairwise_norm$h2_value),, drop = FALSE ]
  h2_pairwise_norm_table <- as.data.frame(h2_pairwise_norm)
  colnames(h2_pairwise_norm_table) <- "H^2 Normalized"

  h2_pairwise_raw <- round(num, 5)
  h2_pairwise_raw <- h2_pairwise_raw[order(-h2_pairwise_raw$h2_value),, drop = FALSE ]
  h2_pairwise_raw_table <- as.data.frame(h2_pairwise_raw)
  colnames(h2_pairwise_raw_table) <- "H^2 Raw"

  h2_pairwise_norm_table <- h2_pairwise_norm_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  h2_pairwise_raw_table <- h2_pairwise_raw_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}


calc_hstats_binary <- function(analysis_object, use_test){

  if (use_test){

    train_data <- analysis_object$data$raw$test_data %>%
      dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  } else {

    train_data <- analysis_object$data$raw$train_data %>%
      dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  }

  hstats_object <- comp_hstats(analysis_object$final_model,
                               train_data,
                               task = "classification",
                               outcome_levels = analysis_object$outcome_levels)

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5))
  colnames(h2_normalized) <- "H^2 Normalized"

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom

  h2_pairwise_norm <- round(num / denom, 5)
  h2_pairwise_norm <- h2_pairwise_norm[order(-h2_pairwise_norm$h2_value), , drop = FALSE]
  h2_pairwise_norm_table <- as.data.frame(h2_pairwise_norm)
  colnames(h2_pairwise_norm_table) <- "H^2 Normalized"

  h2_pairwise_raw <- round(num, 5)
  h2_pairwise_raw <- h2_pairwise_raw[order(-h2_pairwise_raw$h2_value),, drop = FALSE ]
  h2_pairwise_raw_table <- as.data.frame(h2_pairwise_raw)
  colnames(h2_pairwise_raw_table) <- "H^2 Raw"

  h2_pairwise_norm_table <- h2_pairwise_norm_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  h2_pairwise_raw_table <- h2_pairwise_raw_table %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1)

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}


calc_hstats_multiclass <- function(analysis_object, use_test){

  if (use_test){

    train_data <- analysis_object$data$raw$test_data %>%
      dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  } else {

    train_data <- analysis_object$data$raw$train_data %>%
      dplyr::select(-dplyr::all_of(analysis_object$dep_var))

  }

  hstats_object <- comp_hstats_mult(analysis_object$final_model,
                               train_data,
                               task = "classification",
                               outcome_levels = analysis_object$outcome_levels)

  # Total H2

  num   <- hstats_object$h2_overall$num
  denom <- hstats_object$h2_overall$denom
  h2_normalized <- as.data.frame(round(sweep(num, 2, denom, "/"), 5))
  colnames(h2_normalized) <- gsub("^\\.pred_", "", colnames(h2_normalized))

  h2_table <- h2_normalized %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::relocate(Feature, .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -Feature))))

  rownames(h2_table) <- NULL

  # Pairwise H2

  num   <- hstats_object$h2_pairwise$num
  denom <- hstats_object$h2_pairwise$denom
  h2_pairwise_norm <- as.data.frame(round(num / denom, 5))
  colnames(h2_pairwise_norm) <- gsub("^\\.pred_", "",
                                     colnames(h2_pairwise_norm))

  h2_pairwise_raw <- as.data.frame(round(num, 5))
  colnames(h2_pairwise_raw) <- gsub("^\\.pred_", "",
                                     colnames(h2_pairwise_raw))

  h2_pairwise_norm_table <- h2_pairwise_norm %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -"Pairwise Interaction"))))

  h2_pairwise_raw_table <- h2_pairwise_raw %>%
    dplyr::mutate("Pairwise Interaction" = rownames(.)) %>%
    dplyr::relocate("Pairwise Interaction", .before = 1) %>%
    dplyr::arrange(dplyr::desc(rowMeans(dplyr::select(., -"Pairwise Interaction"))))

  rownames(h2_pairwise_norm_table) <- NULL
  rownames(h2_pairwise_raw_table) <- NULL

  return(list(h2_total = h2_table,
              h2_pairwise_norm = h2_pairwise_norm_table,
              h2_pairwise_raw = h2_pairwise_raw_table))

}

hstat_total_plot <- function(h2_total, outcome_levels){

  if (outcome_levels <= 2){

    max_x <- 1.5 * max(h2_total[["H^2 Normalized"]], na.rm = TRUE)

    p <- ggplot2::ggplot(h2_total, ggplot2::aes(x = .data[["H^2 Normalized"]],
                                      y = stats::reorder(Feature, .data[["H^2 Normalized"]]))) +
      ggplot2::geom_col(orientation = "y", fill = "orange") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f",
                                                      .data[["H^2 Normalized"]]),
                                      x = .data[["H^2 Normalized"]] + 0.1*.data[["H^2 Normalized"]]  # slight offset
      ),
      hjust = -0.2,
      size = 3) +

      ggplot2::scale_x_continuous(
        limits = c(0, max_x),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +

      ggplot2::labs(x = expression(H^2~"Normalized"), y = NULL,
           title = "Friedman's H-statistic")

  } else {

    h2_long <- h2_total %>%
      tidyr::pivot_longer(
        cols = -Feature,
        names_to = "Class",
        values_to = "H2"
      )

    max_x <- 1.5 * max(h2_long$H2, na.rm = TRUE)

    h2_long <- h2_long %>%
      dplyr::group_by(Class) %>%
      dplyr::mutate(Feature_ord = stats::reorder(Feature, H2)) %>%
      dplyr::ungroup()

    p <- ggplot2::ggplot(
      h2_long,
      ggplot2::aes(
        x = H2,
        y = Feature_ord,
        fill = Class
      )
    ) +

      ggplot2::geom_col(orientation = "y") +

      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f",
                                                      H2),
                                              x = H2 + 0.1*H2  # slight offset
      ),
      hjust = -0.2,
      size = 3) +

      ggplot2::facet_wrap(~ Class) +

      ggplot2::scale_x_continuous(
        limits = c(0, max_x),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +

      ggplot2::labs(
        x = expression(H^2~"Normalized"),
        y = "Feature",
        title = "Friedman's H-statistic per Class"
      ) +

      ggplot2::theme_grey() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 12, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 8)
      )
  }

  return(p)

}

hstat_pairwise_plot <- function(h2_pairwise, outcome_levels, normalized = TRUE){

    if (normalized){

      h2_col <- "H^2 Normalized"

    } else {

      h2_col <- "H^2 Raw"

    }

    if (outcome_levels <= 2){

      max_x <- 1.5 * max(h2_pairwise[[h2_col]], na.rm = TRUE)

      p <- ggplot2::ggplot(h2_pairwise,
                        ggplot2::aes(x = .data[[h2_col]],
                        y = stats::reorder(.data[["Pairwise Interaction"]],
                                    .data[[h2_col]]))) +
        ggplot2::geom_col(orientation = "y", fill = "orange") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f",
                                                        .data[[h2_col]]),
                                        x = .data[[h2_col]] + 0.1 * .data[[h2_col]]  # slight offset
        ),
        hjust = -0.2,
        size = 3) +
        ggplot2::scale_x_continuous(
          limits = c(0, max_x),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        )

    }

    else {

      h2_long <- h2_pairwise %>%
        tidyr::pivot_longer(
          cols = -'Pairwise Interaction',
          names_to = "Class",
          values_to = "H2"
        )

      max_x <- 1.5 * max(h2_long$H2, na.rm = TRUE)

      p <- ggplot2::ggplot(h2_long,
                        ggplot2::aes(x = H2,
                                  y = stats::reorder(.data[["Pairwise Interaction"]], H2),
                                  fill = Class)) +
        ggplot2::geom_col(orientation = "y") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f",
                                                        H2),
                                        x = H2 + 0.1 * H2
        ),
        hjust = -0.2,
        size = 3) +
        ggplot2::facet_wrap("Class") +
        ggplot2::scale_x_continuous(
          limits = c(0, max_x),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        )


    }

    if (normalized){

      p <- p + ggplot2::labs(x = expression(H^2~"Normalized"), y = "Feature Interaction",
                            title = "Feature Interaction Plot")

    } else {

      p <- p + ggplot2::labs(x = expression(H^2~"Unnormalized"), y = "Feature Interaction",
                             title = "Feature Interaction Plot")

    }

    return(p)

}

##### Comp #####

comp_hstats <- function(model, df, task, grid_size = 20, n_sample = 300, outcome_levels = 0){

  pd <- list()
  num <- list()
  denom <- list()

  df <- df[sample(nrow(df), min(n_sample, nrow(df))), ]

  f_pred <- pred_fun(model, df, task, outcome_levels)


  f_centered <- f_pred - mean(f_pred)

  for (feature in names(df)){

    ice_df <- ice_data(model, df, task, feature,
                       outcome_levels = outcome_levels, grid_size = grid_size)

    pd_j <- ice_df %>%
      dplyr::group_by(feature_value) %>%
      dplyr::summarise(pd = mean(prediction))

    if (is.numeric(df[[feature]])){

      pd_j <- stats::approx(x = pd_j$feature_value,
                            y = pd_j$pd,
                            xout = df[[feature]],
                            rule = 2)$y

    } else {

      levels <- levels(as.factor(df[[feature]]))

      pd_j <- pd_j$pd[ match(df[[feature]], levels) ]

    }

    pd[[feature]] <- pd_j

    pd_minus_j <- ice_df %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(pd_minus_j = mean(prediction))

    pd_j_centered <- pd_j - mean(pd_j)
    pd_minus_j_centered <- pd_minus_j$pd_minus_j - mean(pd_minus_j$pd_minus_j)

    num[[feature]] <- sum((f_centered - pd_j_centered  - pd_minus_j_centered)^2)

  }

  num <- as.data.frame(unlist(num), )
  colnames(num) <- "h2_value"
  rownames(num) <- names(df)

  denom <- sum((f_centered)^2)

  selected_features <- rownames(num)[order(num$h2_value, decreasing = TRUE)][1:min(5, nrow(num))]

  num_ij <- c()

  denom_ij <- c()

  interaction_name_list <- c()

  for (i in 1:length(selected_features)){

    for (j in i:length(selected_features)){

      if (i == j){next}

      feat1 <- selected_features[[i]]
      feat2 <- selected_features[[j]]

      interaction_name <- paste0(feat1,":", feat2)

      h_ij <- hstat_interaction(model, df, feat1, feat2,
                                pd[[feat1]], pd[[feat2]],
                                task = task,
                                outcome_levels = outcome_levels)

      interaction_name_list <- c(interaction_name_list, interaction_name)
      num_ij <- c(num_ij, h_ij$num)
      denom_ij <- c(denom_ij, h_ij$denom)

    }
  }

  num_df <- data.frame(
    h2_value = num_ij
  )

  denom_df <- data.frame(
    h2_value = denom_ij
  )

  rownames(num_df) <- interaction_name_list
  rownames(denom_df) <- interaction_name_list

  ord <- order(num_df$h2_value, decreasing = TRUE)

  # 2. Reorder both dataframes using the index
  num_df   <- num_df[ord, , drop = FALSE]
  denom_df <- denom_df[ord, , drop = FALSE]


  return(list(h2_overall = list(num = num, denom = denom),
              h2_pairwise = list(num = num_df, denom = denom_df))
  )

}

comp_hstats_mult <- function(model, df, task, grid_size = 20, n_sample = 300, outcome_levels = 0){

  pd <- list()
  num <- list()
  denom <- list()

  df <- df[sample(nrow(df), min(n_sample, nrow(df))), ]

  f_pred <- pred_fun(model, df, task, outcome_levels)

  f_centered <- f_pred - colMeans(f_pred)

  denom <- colSums((f_centered)^2)

  dep_levels <- names(f_centered)

  for (feature in names(df)){

    ice_df <- ice_data(model, df, task, feature,
                       outcome_levels = outcome_levels, grid_size = grid_size)

    pd_j <- ice_df %>%
      dplyr::group_by(pred_class, feature_value) %>%
      dplyr::summarise(pd = mean(prediction), .groups = "drop")

    pd_minus_j <- ice_df %>%
      dplyr::group_by(pred_class, id) %>%
      dplyr::summarise(pd_minus_j = mean(prediction), .groups = "drop")

    for (level in dep_levels){

      class_name <- sub("^\\.pred_", "", level)

      pd_j_filter <- pd_j %>% dplyr::filter(pred_class == class_name)
      pd_minus_j_filter <- pd_minus_j %>% dplyr::filter(pred_class == class_name)

      if (is.numeric(df[[feature]])){

        pd_j_filter <- stats::approx(x = pd_j_filter$feature_value,
                                     y = pd_j_filter$pd,
                                     xout = df[[feature]],
                                     rule = 2)$y

      } else {

        cat_levels <- levels(as.factor(df[[feature]]))

        pd_j_filter <- pd_j_filter$pd[ match(df[[feature]], cat_levels) ]

      }

      pd[[level]][[feature]] <- pd_j_filter

      pd_j_centered <- pd_j_filter - mean(pd_j_filter)
      pd_minus_j_centered <- pd_minus_j_filter$pd_minus_j - mean(pd_minus_j_filter$pd_minus_j)

      num[[level]][[feature]] <- sum((f_centered[[level]] - pd_j_centered  - pd_minus_j_centered)^2)

    }

  }

  for (level in dep_levels){

    num[[level]] <- unlist(num[[level]], use.names = FALSE)

  }

  num <- as.data.frame(num)
  colnames(num) <- dep_levels
  rownames(num) <- names(df)

  avg_h2 <- rowMeans(num / denom)

  selected_features <- rownames(num)[order(avg_h2, decreasing = TRUE)][1:min(5, nrow(num))]

  interaction_name_list <- c()

  num_ij <- list()
  denom_ij <- list()

  for (i in 1:length(selected_features)){

    for (j in i:length(selected_features)){

      if (i == j){next}

      feat1 <- selected_features[[i]]
      feat2 <- selected_features[[j]]

      interaction_name <- paste0(feat1,":", feat2)

      for (level in dep_levels){

        h_ij <- hstat_interaction(model, df, feat1, feat2,
                                  pd[[level]][[feat1]], pd[[level]][[feat2]],
                                  task = task,
                                  outcome_levels = outcome_levels,
                                  level = level)

        num_ij[[level]] <- c(num_ij[[level]], h_ij$num)
        denom_ij[[level]] <- c(denom_ij[[level]], h_ij$denom)

      }

      interaction_name_list <- c(interaction_name_list, interaction_name)

    }
  }

  num_df <- as.data.frame(num_ij)
  colnames(num_df) <- dep_levels
  rownames(num_df) <- interaction_name_list

  denom_df <- as.data.frame(denom_ij)
  colnames(denom_df) <- dep_levels
  rownames(denom_df) <- interaction_name_list

  avg_h2 <- rowMeans(num_df[dep_levels] / denom_df[dep_levels])

  ord <- order(avg_h2, decreasing = TRUE)

  # 2. Reorder both dataframes using the index
  num_df   <- num_df[ord, , drop = FALSE]
  denom_df <- denom_df[ord, , drop = FALSE]


  return(list(h2_overall = list(num = num, denom = denom),
              h2_pairwise = list(num = num_df, denom = denom_df))
  )

}

hstat_interaction <- function(model, data, feat1, feat2, pd_j, pd_k,
                              grid.size = 20, task = "regression",
                              outcome_levels = 0,
                              level = NULL){

  xj <- data[[feat1]]
  xk <- data[[feat2]]

  # Determine if numeric or categorical
  is.num1 <- is.numeric(xj)
  is.num2 <- is.numeric(xk)

  pd_2D <- calc_pd_2D(model, data, feat1, feat2, grid.size, task, outcome_levels, level = level)

  if (is.num1 && is.num2){

    pd_jk <- pd2D_interpolate(pd_2D$grid1, pd_2D$grid2, pd_2D$pd.matrix, xj, xk)

  }

  if (!is.num1 && is.num2){

    pd_jk <- pd_1d_interpolate_vec(pd_2D$grid, xj, xk, cat_var = 1)

  }

  if (is.num1 && !is.num2){

    pd_jk <- pd_1d_interpolate_vec(pd_2D$grid, xj, xk, cat_var = 2)

  }

  if (!is.num1 && !is.num2){

    idx <- match(
      paste(data[[feat1]], data[[feat2]]),
      paste(pd_2D$grid[[feat1]], pd_2D$grid[[feat2]])
    )

    pd_jk <- pd_2D$grid$pd[idx]

  }

  pd_jk_centered <- pd_jk - mean(pd_jk)
  pd_j_centered <- pd_j - mean(pd_j)
  pd_k_centered <- pd_k - mean(pd_k)

  num <- sum((pd_jk_centered - pd_j_centered  - pd_k_centered)^2)

  denom <- sum((pd_jk_centered)^2)

  return(list(num = num, denom = denom))


}
