olden_calc <- function(model, task, outcome_levels, y_classes){

  if (task == "regression"){

    net_imp = olden_reg(model)

  }

  else{

    if (outcome_levels == 2){

      net_imp = olden_bin(model)

    } else {

      net_imp = olden_mul(model)

    }

  }

  return(net_imp)

}

olden_reg <- function(model){

  w_1 = coef(model$fit)[[1]]
  w_2 = coef(model$fit)[[3]]

  net_imp = t(w_1) %*% t(w_2)

  norm_factor = sum(abs(net_imp))

  net_imp = net_imp / norm_factor

  return(net_imp)

}

olden_bin <- function(model){

  w_1 = coef(model$fit)[[1]]
  w_2 = coef(model$fit)[[3]]

  imp = t(w_1) %*% t(w_2)

  net_imp = imp[,2] - imp[,1]

  norm_factor = sum(abs(net_imp))

  net_imp = net_imp / norm_factor

  return(net_imp)

}

olden_mul <- function(model){

  w_1 = coef(model$fit)[[1]]
  w_2 = coef(model$fit)[[3]]

  imp = t(w_1) %*% t(w_2)

  return(imp)

}

olden_barplot <- function(net_importance, names_predictor, title = "Olden Feature Importance"){

  df <- data.frame(
    variable = names_predictor,
    importance = as.numeric(net_importance)
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
      y = "Olden Feature Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::coord_cartesian(clip="off")

  return(p)

}
