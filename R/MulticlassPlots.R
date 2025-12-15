plot_multi_pfi <- function(df){

  x_max <- max(df$Importance) * 1.2

  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(Feature, Importance),
                               y = Importance)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Importance - StDev,
                                        ymax = Importance + StDev),
                           width = 0.2,
                           color = "black") +


    # ---- NEW: text labels ----
  ggplot2::geom_text(ggplot2::aes(
    label = sprintf("%.3f (%.3f)", Importance, StDev),
    y = Importance + sign(Importance) * max(abs(StDev)) * 0.2  # slight offset
  ),
  size = 3.2,
  hjust = -0.2) +
    # ---------------------------

  ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ output_class, scales = "free_y") +

    # ---- EXPAND X-AXIS ----
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(add= c(0, x_max))) +

    ggplot2::labs(
      title = "Permutation Feature Importance by Output Class",
      x = "Feature",
      y = "Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )

  return(p)
}


plot_multi_directional <- function(df, title) {

  offset <- 0.05 * max(abs(df$Directional_Importance))

  # ----- FIX -----
  df2 <- df %>%
    dplyr::group_by(output_class) %>%
    dplyr::arrange(output_class, dplyr::desc(Directional_Importance)) %>%
    dplyr::mutate(
      Feature_ord = factor(Feature, levels = Feature),  # niveles fijos por clase
      label_pos = Directional_Importance +
        ifelse(Directional_Importance > 0, offset, -offset),
      vjust_pos = ifelse(Directional_Importance > 0, -0.2, 1.2)
    ) %>%
    dplyr::ungroup()
  # ----------------

  ggplot2::ggplot(df2,
                  ggplot2::aes(
                    x = Feature_ord,
                    y = Directional_Importance,
                    fill = Directional_Importance > 0
                  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = label_pos,
        label = sprintf("%.3f", Directional_Importance),
        vjust = vjust_pos
      ),
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "firebrick")
    ) +
    ggplot2::facet_wrap(~ output_class) +
    ggplot2::labs(
      title = title,
      x = "Feature",
      y = "Feature Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text   = ggplot2::element_text(size = 12, face = "bold")
    )
}

plot_multi_abs <- function(df, y_label, title) {

  x_max <- max(df$Mean_Abs_Importance) * 1.2

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(Feature, Mean_Abs_Importance),
                          y = Mean_Abs_Importance)) +

    ggplot2::geom_col(fill = "steelblue") +

    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = Mean_Abs_Importance - StDev,
      ymax = Mean_Abs_Importance + StDev
    ), width = 0.3) +

    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f (%.3f)",
                                           Mean_Abs_Importance, StDev),
                                    y = Mean_Abs_Importance + sign(Mean_Abs_Importance) * max(abs(StDev)) * 0.2  # slight offset
                                    ),
                       hjust = -0.2,
                       size = 3) +

    ggplot2::coord_flip() +

    ggplot2::facet_wrap(~ output_class, scales = "free_y") +

    # ---- EXPAND X-AXIS ----
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(add= c(0, x_max))) +
    # ------------------------

  ggplot2::labs(
    title = title,
    x = "Feature",
    y = y_label
  ) +

    ggplot2::theme_grey() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    )
}

plot_beeswarm_multi <- function(df, x_label, title) {

  q <- stats::quantile(df$val_color, c(0.02, 0.98), na.rm = TRUE)
  if (diff(q) == 0) q <- q + c(-1e-8, 1e-8)

  ggplot2::ggplot(df, ggplot2::aes(
    x = imp_value,
    y = variable,
    color = val_color
  )) +
    ggbeeswarm::geom_quasirandom(
      bandwidth = 0.20,
      method = "pseudorandom",
      cex = 1.8,
      orientation = "x"
    ) +

    ggplot2::facet_wrap(
      ~ output_class
    ) +

    ggplot2::scale_color_viridis_c(
      option = "A",
      limits = q,
      oob = scales::squish
    ) +

    ggplot2::labs(
      title = title,
      x = x_label,
      y = "Feature"
    ) +

    ggplot2::theme_grey() +
    ggplot2::theme(
      legend.position = "right",

      # ⭐ Smaller + tilted y-axis text
      axis.text.y = ggplot2::element_text(size = 7, angle = 45, hjust = 1),

      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )
}

plot_boxplot_multi <- function(df, y_label,title) {

  ggplot2::ggplot(df, ggplot2::aes(
    x = variable,
    y = imp_value
  )) +

    ggplot2::geom_boxplot(outlier.size = 0.8, alpha = 0.8) +

    ggplot2::facet_wrap(
      ~ output_class
    ) +

    ggplot2::labs(
      title = title,
      x = "Feature",
      y = y_label
    ) +

    ggplot2::theme_grey() +
    ggplot2::theme(
      legend.position = "none",

      # ⭐ Tilt & reduce size of labels
      axis.text.x = ggplot2::element_text(size = 7, angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 7),

      strip.text = ggplot2::element_text(size = 12, face = "bold")
    ) +

    # reorder features from lowest to highest importance
    ggplot2::scale_x_discrete(limits = rev(levels(df$variable)))

}

build_importance_long <- function(results, X_orig, y_classes) {

  # ---------- 1. Build long table for each class ----------
  imp_list <- lapply(
    y_classes,
    function(cls) {
      df <- results[[cls]]
      df$row_id <- seq_len(nrow(df))

      df_long <- tidyr::pivot_longer(
        df,
        -row_id,
        names_to = "variable",
        values_to = "imp_value"    # <--- ALWAYS "imp_value"
      )

      df_long$output_class <- cls
      df_long
    }
  )

  imp_long <- do.call(rbind, imp_list)

  # ---------- 2. Convert original X to long ----------
  X_long <- X_orig |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      -row_id,
      names_to = "variable",
      values_to = "val_color"
    )

  # ---------- 3. Join ----------
  imp_long <- dplyr::left_join(
    imp_long, X_long,
    by = c("row_id", "variable")
  )

  # ---------- 4. Order variables ----------
  order_df <- imp_long |>
    dplyr::group_by(variable) |>
    dplyr::summarise(order_val = mean(abs(imp_value))) |>
    dplyr::arrange(order_val)

  imp_long$variable <- factor(imp_long$variable,
                              levels = order_df$variable)

  return(imp_long)
}

plot_olden_multi <- function(df, title = "Olden Feature Importance") {

  offset <- 0.05 * max(abs(df$Importance))

  df2 <- df %>%
    dplyr::group_by(output_class) %>%
    dplyr::mutate(
      # per-class ordering: max positive → most negative
      Feature_ord = stats::reorder(Feature, -Importance),

      # label placement
      label_pos  = Importance + ifelse(Importance > 0, offset, -offset),
      vjust_pos  = ifelse(Importance > 0, -0.2, 1.2)
    ) %>%
    dplyr::ungroup()

   p <- ggplot2::ggplot(df2,
                  ggplot2::aes(
                    x = Feature_ord,
                    y = Importance,
                    fill = Importance > 0
                  )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        y = label_pos,
        label = sprintf("%.3f", Importance),
        vjust = vjust_pos
      ),
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "firebrick")
    ) +
    ggplot2::facet_wrap(~ output_class, scales = "free_x") +
    ggplot2::labs(
      title = title,
      x = "Feature",
      y = "Importance"
    ) +
    ggplot2::theme_grey() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 7, angle = 45, hjust = 1)
    )

   return(p)

}
