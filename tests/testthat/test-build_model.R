test_that("Check create_nn works properly",{

    hyper_nn_tune_list = list(
      learn_rate = c(-2, -1),
      hidden_units = 15
    )

    hyper_nn_tune = HyperparamsNN$new(hyper_nn_tune_list)
    nn_model = create_nn(hyper_nn_tune, "regression", 10)

    expect_equal(nn_model$mode, "regression")
    expect_equal(class(nn_model), c("mlp", "model_spec"))
    expect_equal(names(nn_model$args), c("hidden_units", "penalty", "dropout", "epochs", "activation", "learn_rate"))

})

test_that("Check create_rf works properly", {

  hyper_rf_tune_list = list(
    mtry = c(3,5),
    trees = 100
  )

  hyper_rf_tune = HyperparamsRF$new(hyper_rf_tune_list)
  rf_model = create_rf(hyper_rf_tune, "classification")

  expect_equal(rf_model$mode, "classification")
  expect_equal(class(rf_model), c("rand_forest", "model_spec"))
  expect_equal(names(rf_model$args), c("mtry", "trees", "min_n"))

})

test_that("Check create_svm works properly", {

  hyper_svm_linear_list = list(cost = c(-2,-1), type = "linear")
  hyper_svm_rbf_list = list(rbf_sigma = c(-2,-1), type = "rbf")
  hyper_svm_poly_list = list(degree = 3, cost = c(-2, -1), type = "poly")

  hyper_svm_linear = HyperparamsSVM$new(hyper_svm_linear_list)
  hyper_svm_rbf = HyperparamsSVM$new(hyper_svm_rbf_list)
  hyper_svm_poly = HyperparamsSVM$new(hyper_svm_poly_list)

  svm_linear = create_svm_linear(hyper_svm_linear, "regression")
  svm_rbf = create_svm_rbf(hyper_svm_rbf, "classification")
  svm_poly = create_svm_poly(hyper_svm_poly, "regression")

  expect_equal(svm_linear$mode, "regression")
  expect_equal(svm_rbf$mode, "classification")
  expect_equal(svm_poly$mode, "regression")

  expect_equal(class(svm_linear), c("svm_linear", "model_spec"))
  expect_equal(class(svm_rbf), c("svm_rbf", "model_spec"))
  expect_equal(class(svm_poly), c("svm_poly", "model_spec"))

  expect_equal(names(svm_linear$args), c("cost", "margin"))
  expect_equal(names(svm_rbf$args), c("cost", "rbf_sigma", "margin"))
  expect_equal(names(svm_poly$args), c("cost", "degree", "scale_factor", "margin"))

})

test_that("Check build_model NN works properly",{

    df = sim_data
    formula = "psych_well ~ ."

    analysis_object <- preprocessing(df = df, formula = formula, task = "regression")

    hyper_nn_tune_list = list(
      learn_rate = c(-2, -1),
      hidden_units = 15
    )


    model_object = build_model(analysis_object = analysis_object,
                             model_name = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

    expect_equal(model_object$task, "regression")
    expect_equal(model_object$model_name, "Neural Network")
    expect_equal(class(model_object$model), c("mlp", "model_spec"))
    expect_equal(model_object$hyperparameters$tuning, T)
    expect_equal(model_object$hyperparameters$learn_rate_tune, T)
    expect_equal(model_object$hyperparameters$hidden_units_tune, F)
    expect_equal(model_object$hyperparameters$activation_tune, T)
    expect_equal(model_object$hyperparameters$hyperparams_constant$hidden_units, 15)
    expect_equal(model_object$hyperparameters$hyperparams_ranges$learn_rate$range, list("lower" = -2, "upper" = -1))
    expect_equal(model_object$hyperparameters$hyperparams_ranges$activation$values, c("relu", "tanh", "sigmoid"))

})

test_that("Check build_model RF works properly",{

  df = sim_data
  formula = "psych_well_bin ~ ."

  analysis_object <- preprocessing(df = df, formula = formula, task = "classification")

  hyper_rf_tune_list = list(
    mtry = c(3,5),
    trees = 100
  )

  model_object = build_model(analysis_object = analysis_object,
                               model_name = "Random Forest",
                               hyperparameters = hyper_rf_tune_list)

  expect_equal(model_object$task, "classification")
  expect_equal(model_object$model_name, "Random Forest")
  expect_equal(class(model_object$model), c("rand_forest", "model_spec"))
  expect_equal(model_object$hyperparameters$tuning, T)
  expect_equal(model_object$hyperparameters$mtry_tune, T)
  expect_equal(model_object$hyperparameters$trees_tune, F)
  expect_equal(model_object$hyperparameters$min_n_tune, T)
  expect_equal(model_object$hyperparameters$hyperparams_constant$trees, 100)
  expect_equal(model_object$hyperparameters$hyperparams_ranges$mtry$range, list("lower" = 3, "upper" = 5))
  expect_equal(model_object$hyperparameters$hyperparams_ranges$min_n$range, list(lower = 5, upper = 25))

})

test_that("Check build_model SVM works properly",{

  df = sim_data
  formula = "psych_well_bin ~ ."

  analysis_object <- preprocessing(df = df, formula = formula, task = "classification")

  hyper_svm_tune_list = list(
    type = "rbf",
    cost = -2,
    rbf_sigma = c(-3,-1)
  )

  model_object = build_model(analysis_object = analysis_object,
                               model_name = "SVM",
                               hyperparameters = hyper_svm_tune_list)

  expect_equal(model_object$task, "classification")
  expect_equal(model_object$model_name, "SVM")
  expect_equal(class(model_object$model), c("svm_rbf", "model_spec"))
  expect_equal(model_object$hyperparameters$tuning, T)
  expect_equal(model_object$hyperparameters$rbf_sigma_tune, T)
  expect_equal(model_object$hyperparameters$cost_tune, F)
  expect_equal(model_object$hyperparameters$margin_tune, T)
  expect_equal(model_object$hyperparameters$hyperparams_constant$cost, -2)
  expect_equal(model_object$hyperparameters$hyperparams_ranges$rbf_sigma$range, list("lower" = -3, "upper" = -1))
  expect_equal(model_object$hyperparameters$hyperparams_ranges$margin$range, list(lower = 0, upper = 0.2))

})

test_that("Check build_model wrong model name",{

  df = sim_data
  formula = "psych_well ~ ."

  analysis_object <- preprocessing(df = df, formula = formula, task = "regression")

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = 15
  )

  expect_error(build_model(analysis_object = analysis_object,
                             model_name = "Neural Networkz",
                             hyperparameters = hyper_nn_tune_list))
})

test_that("Check build_model wrong hyperparameters name",{

  df = sim_data
  formula = "psych_well ~ ."

  analysis_object <- preprocessing(df = df, formula = formula, task = "regression")

  hyper_nn_tune_list = list(
    learn_rates = c(-2, -1),
    hidden_units = 15
  )

  expect_error(build_model(analysis_object = analysis_object,
                           model_name = "Neural Network",
                           hyperparameters = hyper_nn_tune_list))
})


