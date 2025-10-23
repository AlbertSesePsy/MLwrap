formula_bin <- "psych_well_bin ~ age + gender + depression"
formula_reg <- "psych_well ~ age + gender + depression"

analysis_object_reg = preprocessing(df = sim_data, formula_reg, task = "regression")


analysis_object_bin = preprocessing(df = sim_data, formula_bin, task = "classification")


hyper_nn_tune_list = list(

  learn_rate = c(-2, -1),
  hidden_units = c(3,10)

)

hyper_rf_tune_list <- list(
  mtry = c(2,6),
  trees = 100
)

model_object_bin = build_model(analysis_object = analysis_object_bin,
                                 model_name = "Neural Network",
                                 hyperparameters = hyper_nn_tune_list)

model_object_reg = build_model(analysis_object = analysis_object_reg,
                                 model_name = "Random Forest",
                                 hyperparameters = hyper_rf_tune_list)

### create_workflow

test_that("Check create_workflow works properly", {

  workflow_bin = create_workflow(model_object_bin)
  workflow_reg = create_workflow(model_object_reg)

  expect_equal(class(workflow_bin$pre$actions$recipe), c("action_recipe", "action_pre", "action"))
  expect_equal(class(workflow_reg$pre$actions$recipe), c("action_recipe", "action_pre", "action"))

})

###### split_data

test_that("Check split_data works properly", {

  model_object_bin$modify("tuner", "Bayesian Optimization")
  model_object_reg$modify("tuner", "Grid Search CV")

  split_data_bin <- split_data(model_object_bin)
  split_data_reg <- split_data(model_object_reg)

  expect_equal(class(split_data_bin$sampling_method$splits[[1]]), c("vfold_split", "rsplit"))
  expect_equal(split_data_bin$final_split, rbind(model_object_bin$train_data, model_object_bin$validation_data))

  expect_equal(class(split_data_reg$sampling_method$splits[[1]]), c("vfold_split", "rsplit") )
  expect_equal(split_data_reg$final_split, model_object_reg$train_data)

})


##### create_metric_set

test_that("Test create_metric_set works properly", {

  metrics_bin = c("roc_auc", "accuracy")
  metrics_reg = c("rmse", "ccc")

  model_object_bin$modify("metrics", metrics_bin)
  model_object_reg$modify("metrics", metrics_reg)

  metric_set_bin <- create_metric_set(model_object_bin$metrics)
  metric_set_reg <- create_metric_set(model_object_reg$metrics)

  expect_equal(class(metric_set_bin), c("class_prob_metric_set", "metric_set", "function"))
  expect_equal(class(metric_set_reg), c("numeric_metric_set", "metric_set", "function"))

})

###### extract_hyperparams

test_that("Test extract_hyperparams works properly", {

  model_object_bin2 = model_object_bin$clone()
  model_object_reg2 = model_object_reg$clone()

  model_object_bin2$modify("workflow", create_workflow(model_object_bin))
  model_object_reg2$modify("workflow", create_workflow(model_object_reg))

  extracted_hyp_bin <- extract_hyperparams(model_object_bin2)
  extracted_hyp_reg <- extract_hyperparams(model_object_reg2)

  expect_equal(extracted_hyp_bin$name, c("hidden_units", "activation", "learn_rate"))
  expect_equal(length(extracted_hyp_bin$object), 3)

  expect_equal(extracted_hyp_reg$name, c("mtry", "min_n"))
  expect_equal(length(extracted_hyp_reg$object), 2)

})

##### hyperparams_grid

test_that("Test hyperparams_grid works properly", {

  hyp_rf = HyperparamsRF$new(hyper_rf_tune_list)
  grid = hyperparams_grid(hyp_rf, levels = 5)

  expect_equal(names(grid), c("mtry", "min_n"))
  expect_equal(nrow(grid), 25)

})
