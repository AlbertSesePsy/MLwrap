all_tune_list = list(

  hidden_units = c(5, 50),
  learn_rate = c(-3,-1),
  activation = c("relu", "sigmoid", "tanh")

)

some_tune_list = list(

  hidden_units = c(10,50),
  learn_rate = 0.001,
  activation = c("relu", "sigmoid")

)

no_tune_list = list(

  hidden_units = 50,
  learn_rate = 0.01,
  activation = "relu"

)

test_that("NN Default Hyperparams works", {

  hyperparams_nn = HyperparamsNN$new()

  expect_equal(hyperparams_nn$tuning, T)

  expect_equal(hyperparams_nn$hyperparams_ranges$hidden_units$range, list(lower = 5, upper = 20))
  expect_equal(hyperparams_nn$hyperparams_ranges$learn_rate$range, list(lower = -3, upper = -1.))
  expect_equal(hyperparams_nn$hyperparams_ranges$activation$value, c("relu", "tanh", "sigmoid"))

  expect_equal(hyperparams_nn$hidden_units_tune, T)
  expect_equal(hyperparams_nn$learn_rate_tune, T)
  expect_equal(hyperparams_nn$activation_tune, T)


})

test_that("NN Hyperparams works with all_tune", {

  hyperparams_nn = HyperparamsNN$new(all_tune_list)

  expect_equal(hyperparams_nn$tuning, T)

  expect_equal(hyperparams_nn$hyperparams_ranges$hidden_units$range, list(lower = 5, upper = 50))
  expect_equal(hyperparams_nn$hyperparams_ranges$learn_rate$range, list(lower = -3, upper = -1))
  expect_equal(hyperparams_nn$hyperparams_ranges$activation$value, c("relu", "sigmoid", "tanh"))

  expect_equal(hyperparams_nn$hidden_units_tune, T)
  expect_equal(hyperparams_nn$learn_rate_tune, T)
  expect_equal(hyperparams_nn$activation_tune, T)

})

test_that("NN Hyperparams works with some_tune", {

  hyperparams_nn = HyperparamsNN$new(some_tune_list)

  expect_equal(hyperparams_nn$tuning, T)

  expect_equal(hyperparams_nn$hyperparams_ranges$hidden_units$range, list(lower = 10, upper = 50))
  expect_equal(hyperparams_nn$hyperparams_ranges$activation$value, c("relu", "sigmoid"))

  expect_equal(hyperparams_nn$hyperparams_constant$learn_rate, 0.001)

  expect_equal(hyperparams_nn$hidden_units_tune, T)
  expect_equal(hyperparams_nn$learn_rate_tune, F)
  expect_equal(hyperparams_nn$activation_tune, T)

})

test_that("NN Hyperparams works with no_tune", {

  hyperparams_nn = HyperparamsNN$new(no_tune_list)

  expect_equal(hyperparams_nn$tuning, F)

  expect_equal(hyperparams_nn$hyperparams_constant$hidden_units, 50)
  expect_equal(hyperparams_nn$hyperparams_constant$learn_rate, 0.01)
  expect_equal(hyperparams_nn$hyperparams_constant$activation, "relu")

  expect_equal(hyperparams_nn$hidden_units_tune, F)
  expect_equal(hyperparams_nn$learn_rate_tune, F)
  expect_equal(hyperparams_nn$activation_tune, F)

})

test_that("Check NN Incompatible Hyperparameters Errors", {

  hyp_list = list(

    hidden_units = 4,
    layers = 3

  )

  expect_error(HyperparamsNN$new(hyp_list))


})
