comp_ale <- function(model, X, feature, K = 20,
                     group = NULL, task = "regression",
                     outcome_levels = 1) {

  pred_fun <- function(model, new_data){
    if (task == "regression")
      return(predict(model, new_data = new_data, type = "numeric")[[1]])
    if (task == "classification" && outcome_levels == 2)
      return(predict(model, new_data = new_data, type = "prob")[[2]])
    if (task == "classification" && outcome_levels > 2)
      return(as.data.frame(predict(model, new_data = new_data, type = "prob")))
  }

  # ---- GROUPING MODE ----
  if (!is.null(group)) {
    grp <- X[[group]]

    if (is.numeric(grp)) {
      r <- rank(grp, na.last = "keep") / sum(!is.na(grp))
      grp_vec <- cut(r, breaks = c(0, .25, .50, .75, 1),
                     include.lowest = TRUE, labels = paste0("Q", 1:4))
    } else {
      grp_vec <- grp
    }

    groups <- unique(grp_vec)

    # compute ALE per group → each becomes long-format
    out <- lapply(groups, function(g) {
      df_sub <- X[grp_vec == g, , drop = FALSE]
      ale_df <- comp_ale(model, df_sub, feature, K, group = NULL,
                         task = task, outcome_levels = outcome_levels)
      ale_df$Level <- as.character(g)
      ale_df
    })

    return(dplyr::bind_rows(out))
  }

  # ---- MAIN ALE (NO GROUP) ----
  x <- X[[feature]]
  qs <- stats::quantile(x, seq(0, 1, length.out = K + 1))

  # storage for all classes
  ale_vals <- matrix(0, nrow = K, ncol = max(1, outcome_levels))
  class_names <- NULL

  for (k in 1:K) {
    lo <- qs[k]
    hi <- qs[k + 1]

    idx <- which(x >= lo & x <= hi)
    if (length(idx) == 0) next

    X_lo <- X[idx, , drop = FALSE]
    X_hi <- X_lo
    X_lo[[feature]] <- lo
    X_hi[[feature]] <- hi

    pred_lo <- pred_fun(model, X_lo)
    pred_hi <- pred_fun(model, X_hi)

    # regression or binary → numeric vector
    if (is.numeric(pred_lo) && is.numeric(pred_hi)) {
      ale_vals[k, 1] <- mean(pred_hi - pred_lo)
      class_names <- "Prediction"

    } else {
      # multiclass → matrix
      diff <- as.matrix(pred_hi) - as.matrix(pred_lo)
      ale_vals[k, ] <- colMeans(diff)
      class_names <- sub("^\\.pred_", "", colnames(pred_hi))
    }
  }

  # accumulate per class
  if (length(class_names) == 1) {
    ale_acc <- matrix(cumsum(ale_vals[,1]), ncol = 1)
  } else {
    ale_acc <- apply(ale_vals, 2, cumsum)
  }

  # center per class
  ale_centered <- sweep(ale_acc, 2, colMeans(ale_acc), FUN = "-")

  ale_centered <- unname(ale_centered)
  qs_vals <- unname(qs[-1])
  class_names <- unname(class_names)

  # return long format
  out <- data.frame(
    grid = rep(qs[-1], times = length(class_names)),
    ale  = as.vector(ale_centered),
    Class = rep(class_names, each = K),
    Level = NA,
    row.names = NULL
  )

  return(out)
}
