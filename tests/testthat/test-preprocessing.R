test_that("subset_data works properly", {

  formula = "y ~ x1 + x2 + x3 + x4"
  formula = as.formula(formula)

  df <- data.frame(
    y = as.factor(c(1,0)),
    y2 = factor(c(0,2), levels = c(0,1,2)),
    x1 = c(2,3),
    x2 = c(4,6),
    x3 = as.factor(c(0,1)),
    x4 = factor(c(0,2), levels = c(0,1,2))
  )

  sub_df = subset_data(formula = formula, data = df)

  full_df = subset_data(formula = as.formula("y ~ ."), data = df)

  expect_equal(ncol(sub_df), 5)
  expect_equal(ncol(full_df), 6)

})

test_that("standarize works correctly",{

  formula = "y ~ x1 + x2 + x3 + x4"
  formula = as.formula(formula)

  df <- data.frame(
    y = as.factor(c(1,0)),
    y2 = factor(c(0,2), levels = c(0,1,2)),
    x1 = c(2,3),
    x2 = c(4,6),
    x3 = as.factor(c(0,1)),
    x4 = factor(c(0,2), levels = c(0,1,2))
  )

  rec = recipes::recipe(formula = formula, data = df)

  # For "all":

  rec_all <- standarize_predictors(rec = rec, norm_num_vars = "all")

  rec_all_prep <- recipes::prep(rec_all, training = df)

  rec_all_bake <- recipes::bake(rec_all_prep, new_data = df)

  expect_equal(rec_all_bake$x1, c(-0.5*sqrt(2), 0.5*sqrt(2)), tolerance=1e-3)

  expect_equal(rec_all_bake$x2, c(-1/sqrt(2), 1/sqrt(2)), tolerance=1e-3)

  # For x1:

  rec_x1 <- standarize_predictors(rec = rec, norm_num_vars = c("x1"))

  rec_x1_prep <- recipes::prep(rec_x1, training = df)

  rec_x1_bake <- recipes::bake(rec_x1_prep, new_data = df)

  expect_equal(rec_x1_bake$x1, c(-0.5*sqrt(2), 0.5*sqrt(2)), tolerance=1e-3)

  expect_equal(rec_x1_bake$x2, df$x2, tolerance=1e-3)

})

test_that("one_hot_predictors works correctly",{

  formula = "y ~ x1 + x2 + x3 + x4"
  formula = as.formula(formula)

  df <- data.frame(
    y = as.factor(c(1,0)),
    y2 = factor(c(0,2), levels = c(0,1,2)),
    x1 = c(2,3),
    x2 = c(4,6),
    x3 = as.factor(c(0,1)),
    x4 = factor(c(0,2), levels = c(0,1,2))
  )

  rec = recipes::recipe(formula = formula, data = df)

  # With "all":

  rec_all <- one_hot_predictors(rec = rec, encode_cat_vars = "all")

  rec_all_prep <- recipes::prep(rec_all, training = df)

  rec_all_bake <- recipes::bake(rec_all_prep, new_data = df)

  expect_equal(rec_all_bake$x3_X0, c(1,0))

  expect_equal(rec_all_bake$x3_X1, c(0,1))

  expect_equal(rec_all_bake$x4_X0, c(1,0))

  expect_equal(rec_all_bake$x4_X1, c(0,0))

  expect_equal(rec_all_bake$x4_X2, c(0,1))

  # With only x4:

  rec_all_x4 <- one_hot_predictors(rec = rec, encode_cat_vars = c("x4"))

  rec_all_prep_x4 <- recipes::prep(rec_all_x4, training = df)

  rec_all_bake_x4 <- recipes::bake(rec_all_prep_x4, new_data = df)

  expect_equal(rec_all_bake_x4$x3, df$x3)

  expect_equal(rec_all_bake_x4$x4_X0, c(1,0))

  expect_equal(rec_all_bake_x4$x4_X1, c(0,0))

  expect_equal(rec_all_bake_x4$x4_X2, c(0,1))

})


