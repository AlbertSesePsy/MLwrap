ice_data <- function(model, data, task, feature,
                     outcome_levels = NULL, group_by = NULL, grid_size = 25) {

  # 1) Build trimmed numeric grid (1%–99%)
  x <- data[[feature]]
  if (is.numeric(x)){

    q <- stats::quantile(x, probs = c(0.001, 0.999), na.rm = TRUE)
    grid <- seq(q[1], q[2], length.out = grid_size)

  } else{

    grid <- levels(as.factor(x))

  }

  # 2) Prepare grouping vector (with numeric -> quartiles)
  group_vec <- NULL
  if (!is.null(group_by)) {
    stopifnot(group_by %in% names(data))
    g <- data[[group_by]]
    if (is.numeric(g)) {
      r <- rank(g, na.last = "keep", ties.method = "average") / sum(!is.na(g))
      group_vec <- cut(r, breaks = c(0, .25, .50, .75, 1), include.lowest = TRUE,
                       labels = paste0("Q", 1:4))
    } else {
      group_vec <- g
    }
  }

  # 3) Loop over grid, predict, and build rows
  ice_list <- lapply(grid, function(val) {
    xtemp <- data
    xtemp[[feature]] <- val

    if (identical(task, "regression")) {
      preds <- predict(model, new_data = xtemp)[[1]]

      df <- data.frame(
        id = seq_len(nrow(data)),
        feature_value = val,
        prediction = preds
      )

    } else if (!is.null(outcome_levels) && outcome_levels == 2) {
      # Binary classification: take prob of 2nd level as in your setup
      preds <- predict(model, new_data = xtemp, type = "prob")[[2]]

      df <- data.frame(
        id = seq_len(nrow(data)),
        feature_value = val,
        prediction = preds
      )

    } else {
      # Multiclass: get prob tibble and pivot longer
      prob_tbl <- predict(model, new_data = xtemp, type = "prob")
      # ensure it's a data.frame
      prob_df <- as.data.frame(prob_tbl)

      # names typically like ".pred_classA", ".pred_classB", ...
      cls_names <- sub("^\\.pred_", "", colnames(prob_df))

      # build long data (base-R, no tidyr required)
      id_vec <- rep(seq_len(nrow(prob_df)), times = ncol(prob_df))
      class_vec <- rep(cls_names, each = nrow(prob_df))
      pred_vec <- as.numeric(as.matrix(prob_df))

      df <- data.frame(
        id = id_vec,
        feature_value = val,
        pred_class = class_vec,
        prediction = pred_vec
      )
    }

    # attach grouping if requested
    if (!is.null(group_by)) {
      df[[group_by]] <- rep(group_vec, each = if ("class" %in% names(df)) 1 else 1)
    }

    df
  })

  ice_df <- do.call(rbind, ice_list)
  rownames(ice_df) <- NULL
  ice_df
}

pred_fun <- function(model, new_data, task, outcome_levels){

  if (task == "regression"){

    return(predict(model, new_data= new_data, type = "numeric")[[1]])

  }

  if (task == "classification" && outcome_levels == 2){

    return(predict(model, new_data= new_data, type = "prob")[[2]])

  }

  if (task == "classification" && outcome_levels != 2){

    return(predict(model, new_data= new_data, type = "prob"))

  }

}




calc_pd_2D <- function(model, data, feat1, feat2,
                 grid.size = 20, task = "regression", outcome_levels = 0,
                 level = NULL) {

  x1 <- data[[feat1]]
  x2 <- data[[feat2]]

  # Determine if numeric or categorical
  is.num1 <- is.numeric(x1)
  is.num2 <- is.numeric(x2)

  # ---- 1. BUILD GRID ----
  grid1 <- if (is.num1) seq(min(x1), max(x1), length.out = grid.size) else levels(as.factor(x1))
  grid2 <- if (is.num2) seq(min(x2), max(x2), length.out = grid.size) else levels(as.factor(x2))

  # Cartesian grid
  grid <- expand.grid(val1 = grid1, val2 = grid2, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  colnames(grid) <- c(feat1, feat2)
  # ---- 2. COMPUTE 2D ICE (over the grid) ----
  # For each (val1, val2), replace feat1 & feat2 and predict
  preds <- numeric(nrow(grid))

  for (g in seq_len(nrow(grid))) {
    temp <- data
    temp[[feat1]] <- if (is.num1) grid[[feat1]][g] else factor(grid[[feat1]][g], levels = grid1)
    temp[[feat2]] <- if (is.num2) grid[[feat2]][g] else factor(grid[[feat2]][g], levels = grid2)



    p <- pred_fun(model, temp, task, outcome_levels)

    if (!is.null(level)){

      p <- p[[level]]

    }

    preds[g] <- mean(p)   # partial dependence = average prediction
  }

  grid$pd <- preds

  # ---- 3. RESHAPE TO MATRIX FORM FOR INTERPOLATION OR PLOTTING ----
  # For numeric × numeric → produce matrix grid.size × grid.size
  if (is.num1 && is.num2) {
    pd.mat <- matrix(grid$pd, nrow = length(grid1), ncol = length(grid2), byrow = FALSE)
    out <- list(
      pd.matrix = pd.mat,
      grid = grid,
      grid1 = grid1,
      grid2 = grid2
    )
    return(out)
  }

  # For numeric × categorical
  if (is.num1 && !is.num2) {
    pd.mat <- matrix(grid$pd, nrow = length(grid1), ncol = length(grid2), byrow = FALSE)
    colnames(pd.mat) <- grid2
    rownames(pd.mat) <- grid1

    out <- list(
      pd.matrix = pd.mat,
      grid = grid,
      grid1 = grid1,
      grid2 = grid2
    )
    return(out)
  }

  # For categorical × numeric
  if (!is.num1 && is.num2) {
    pd.mat <- matrix(grid$pd, nrow = length(grid1), ncol = length(grid2), byrow = FALSE)
    rownames(pd.mat) <- grid1
    colnames(pd.mat) <- grid2
    out <- list(
      pd.matrix = pd.mat,
      grid = grid,
      grid1 = grid1,
      grid2 = grid2
    )
    return(out)
  }

  # For factor × factor, no interpolation needed
  if (!is.num1 && !is.num2) {
    pd.mat <- matrix(grid$pd, nrow = length(grid1), ncol = length(grid2), byrow = TRUE,
                     dimnames = list(grid1, grid2))
    out <- list(
      pd.matrix = pd.mat,
      grid = grid,
      grid1 = grid1,
      grid2 = grid2
    )
    return(out)
  }
}

pd2D_interpolate <- function(grid_j, grid_k, pd_mat, xj, xk) {

  # find interval indices for each observation
  find_interval <- function(x, grid) {
    pmax(1, pmin(length(grid) - 1, findInterval(x, grid)))
  }

  j1 <- find_interval(xj, grid_j)
  j2 <- j1 + 1

  k1 <- find_interval(xk, grid_k)
  k2 <- k1 + 1

  # compute interpolation weights
  wj <- (xj - grid_j[j1]) / (grid_j[j2] - grid_j[j1])
  wk <- (xk - grid_k[k1]) / (grid_k[k2] - grid_k[k1])

  # extract corner values
  Q11 <- pd_mat[cbind(j1, k1)]
  Q21 <- pd_mat[cbind(j2, k1)]
  Q12 <- pd_mat[cbind(j1, k2)]
  Q22 <- pd_mat[cbind(j2, k2)]

  # bilinear interpolation
  out <-
    Q11 * (1 - wj) * (1 - wk) +
    Q21 * (wj)      * (1 - wk) +
    Q12 * (1 - wj) * (wk) +
    Q22 * (wj)      * (wk)

  return(out)
}

pd_1d_interpolate <- function(grid, xj, xk, cat_var = 1){

  if (cat_var == 1){

    filter_pd <- grid %>%
                  dplyr::filter(grid[[1]] == xj)

    num_var = 2

    x_out = xk

  } else{

    filter_pd <- grid %>%
      dplyr::filter(grid[[2]] == xk)

    num_var = 1

    x_out = xj

  }

  pd_jk_i <- stats::approx(x = filter_pd[[num_var]],
                        y = filter_pd$pd,
                        xout = x_out,
                        rule = 2)$y

  return(pd_jk_i)

}

pd_1d_interpolate_vec <- function(grid, xj, xk, cat_var = 1) {
  mapply(
    function(a, b) pd_1d_interpolate(grid, a, b, cat_var = cat_var),
    xj, xk
  )
}




