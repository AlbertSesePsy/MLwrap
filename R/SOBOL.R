sobol_calc <- function(model, train, task, feature_names){

  ncols = ncol(train) - 1

  X1 <- as.data.frame(matrix(stats::rnorm(1000 * ncols * ncols), ncol = ncols))
  X2 <- as.data.frame(matrix(stats::rnorm(1000 * ncols * ncols), ncol = ncols))

  names(X1) <- feature_names
  names(X2) <- feature_names



  if (task == "regression"){

    func_model_reg <- function(X) {

      predict(model, new_data = X)$.pred

    }

    res <- sensitivity::soboljansen(model = func_model_reg, X1, X2, nboot = 100, conf = 0.95)

  } else{

    func_model_bin <- function(X) {

      predict(model, new_data = X, type = "prob")[,2][[1]]

    }

    res <- sensitivity::soboljansen(model = func_model_bin, X1, X2, nboot = 100, conf = 0.95)

  }

  return(res)

}

sobol_plot <- function(sobol_result) {

  first_order <- sobol_result$S
  total_order <- sobol_result$T

  df_plot <- data.frame(
    variable = rownames(first_order),
    S1 = first_order$original,
    S1_se = first_order$`std. error`,
    ST = total_order$original,
    ST_se = total_order$`std. error`
  )

  # Order
  df_plot <- df_plot[order(df_plot$ST, decreasing = TRUE), ]
  df_plot$variable <- factor(df_plot$variable, levels = df_plot$variable)

  # Pivot
  df_long <- df_plot %>%
    tidyr::pivot_longer(cols = c(S1, ST), names_to = "type", values_to = "value")

  df_long$se <- c(df_plot$S1_se, df_plot$ST_se)

  df_long$label <- paste0(signif(df_long$value, 3), " +/- ", signif(df_long$se, 1))

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = value, y = variable, fill = type)) +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge(width = 0.7),
                      width = 0.6) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = value - se, xmax = value + se),
                            position = ggplot2::position_dodge(width = 0.7),
                            height = 0.2,
                            color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       position = ggplot2::position_dodge(width = 0.7),
                       hjust = -0.3,
                       linewidth = 3.5) +
    ggplot2::labs(title = "Sobol Indices (First Order vs Total)",
                  x = "Sobol index", y = "Variable") +
    ggplot2::scale_fill_manual(values = c("S1" = "steelblue", "ST" = "darkorange"),
                               labels = c("First order", "Total order"),
                               name = "Type") +
    ggplot2::xlim(0, max(df_long$value + df_long$se) * 1.2) +  # espacio para etiquetas
    ggplot2::theme_minimal()

  return(p)

}
