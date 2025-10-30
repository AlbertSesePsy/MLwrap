##### EXPECT RF OUTPUT REPRODUCIBLE

all_tune_list = list(

  mtry = c(3,8),
  trees = c(50, 200),
  min_n = c(3, 6)

)

no_tune_list = list(

  mtry = 3,
  trees = 100,
  min_n = 4

)

some_tune_list = list(

  mtry = c(3, 5),
  trees = 200,
  min_n = c(4, 8)

)

test_that("Check Random Forest Defualt Hyperparams works", {

  hyperparam_rf = HyperparamsRF$new()

  expect_equal(hyperparam_rf$tuning, T)
  expect_equal(hyperparam_rf$hyperparams_ranges$mtry$range, list(lower = 3, upper = 8))
  expect_equal(hyperparam_rf$hyperparams_ranges$trees$range, list(lower = 100, upper = 300))
  expect_equal(hyperparam_rf$hyperparams_ranges$min_n$range, list(lower = 5, upper = 25))

  expect_equal(hyperparam_rf$mtry_tune, T)
  expect_equal(hyperparam_rf$trees_tune, T)
  expect_equal(hyperparam_rf$min_n_tune, T)


})

test_that("Check Random Forest Hyperparams all_tune", {


          hyperparam_rf = HyperparamsRF$new(all_tune_list)

          expect_equal(hyperparam_rf$tuning, T)
          expect_equal(hyperparam_rf$hyperparams_ranges$mtry$range, list(lower = 3, upper = 8))
          expect_equal(hyperparam_rf$hyperparams_ranges$trees$range, list(lower = 50, upper = 200))
          expect_equal(hyperparam_rf$hyperparams_ranges$min_n$range, list(lower = 3, upper = 6))

          expect_equal(hyperparam_rf$mtry_tune, T)
          expect_equal(hyperparam_rf$trees_tune, T)
          expect_equal(hyperparam_rf$min_n_tune, T)

})

test_that("Check Random Forest Hyperparams some_tune", {


  hyperparam_rf = HyperparamsRF$new(some_tune_list)

  expect_equal(hyperparam_rf$tuning, T)
  expect_equal(hyperparam_rf$hyperparams_ranges$mtry$range, list(lower = 3, upper = 5))
  expect_equal(hyperparam_rf$hyperparams_ranges$min_n$range, list(lower = 4, upper = 8))

  expect_equal(hyperparam_rf$hyperparams_constant$trees, 200)

  expect_equal(hyperparam_rf$mtry_tune, T)
  expect_equal(hyperparam_rf$trees_tune, F)
  expect_equal(hyperparam_rf$min_n_tune, T)


})

test_that("Check Random Forest Hyperparams min_n_tune", {


  hyperparam_rf = HyperparamsRF$new(no_tune_list)

  expect_equal(hyperparam_rf$tuning, F)

  expect_equal(hyperparam_rf$hyperparams_constant, list(

    mtry = 3,
    trees = 100,
    min_n = 4

  ))

  expect_equal(hyperparam_rf$mtry_tune, F)
  expect_equal(hyperparam_rf$trees_tune, F)
  expect_equal(hyperparam_rf$min_n_tune, F)


})

test_that("Check Random Forest Incompatible Hyperparams Error", {

  hyp_list = list(

    metry = 5,
    trees = 100

  )

  expect_error(HyperparamsRF$new(hyp_list))

})
