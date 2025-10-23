test_that("Check Linear SVM Default Hyperparams works",{

  hyp_svm = HyperparamsSVM$new(list(type = "linear"))

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -3, upper = 3))
  expect_equal(hyp_svm$hyperparams_ranges$margin$range, list(lower = 0, upper = 0.2))

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, T)


})

test_that("Check RBF SVM Default Hyperparams works",{

  hyp_svm = HyperparamsSVM$new(list(type = "rbf"))

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -3, upper = 3))
  expect_equal(hyp_svm$hyperparams_ranges$margin$range, list(lower = 0, upper = 0.2))
  expect_equal(hyp_svm$hyperparams_ranges$rbf_sigma$range, list(lower = -5, upper = 0))

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, T)
  expect_equal(hyp_svm$rbf_sigma_tune, T)


})

test_that("Check Polynomial SVM Default Hyperparams works",{

  hyp_svm = HyperparamsSVM$new(list(type = "poly"))

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -3, upper = 3))
  expect_equal(hyp_svm$hyperparams_ranges$margin$range, list(lower = 0, upper = 0.2))
  expect_equal(hyp_svm$hyperparams_ranges$degree$range, list(lower = 1, upper = 3))
  expect_equal(hyp_svm$hyperparams_ranges$scale_factor$range, list(lower = -5, upper = -1))

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, T)
  expect_equal(hyp_svm$degree_tune, T)
  expect_equal(hyp_svm$scale_factor_tune, T)



})

test_that("Check Linear SVM works for some_tune", {

  hyp_linear = list(
                  cost = c(-2,2),
                  margin = -1,
                  type = "linear"
                  )

  hyp_svm = HyperparamsSVM$new(hyp_linear)

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -2, upper = 2))
  expect_equal(hyp_svm$hyperparams_constant$margin, -1)

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, F)

})

test_that("Check RBF SVM works for some_tune", {

  hyp_rbf = list(
    cost = c(-2,2),
    margin = -1,
    rbf_sigma = c(-3, 2),
    type = "rbf"
  )

  hyp_svm = HyperparamsSVM$new(hyp_rbf)

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -2, upper = 2))
  expect_equal(hyp_svm$hyperparams_constant$margin, -1)
  expect_equal(hyp_svm$hyperparams_ranges$rbf_sigma$range, list(lower = -3, upper = 2))

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, F)
  expect_equal(hyp_svm$rbf_sigma_tune, T)

})

test_that("Check Polynomial SVM works for some_tune", {

  hyp_rbf = list(
    cost = c(-2,2),
    margin = -1,
    degree = 3,
    scale_factor = c(-2, -1),
    type = "poly"
  )

  hyp_svm = HyperparamsSVM$new(hyp_rbf)

  expect_equal(hyp_svm$tuning, T)
  expect_equal(hyp_svm$hyperparams_ranges$cost$range, list(lower = -2, upper = 2))
  expect_equal(hyp_svm$hyperparams_constant$margin, -1)
  expect_equal(hyp_svm$hyperparams_constant$degree, 3)
  expect_equal(hyp_svm$hyperparams_ranges$scale_factor$range, list(lower = -2, upper = -1))

  expect_equal(hyp_svm$cost_tune, T)
  expect_equal(hyp_svm$margin_tune, F)
  expect_equal(hyp_svm$degree_tune, F)
  expect_equal(hyp_svm$scale_factor_tune, T)

})

test_that("Check SVM Incompatible Hyperparameters Errors", {

  hyp_list_typo = list(
              margin = -1,
              cost = c(1,3),
              rbf_sigma2 = 3,
              type = "rbf"
  )

  hyp_list_incompatible = list(
    margin = -1,
    cost = c(1,3),
    degree = 3,
    type = "rbf"
  )

  hyp_list_no_type = list(
    margin = -1,
    cost = c(1,3),
    degree = 3
  )

  expect_error(HyperparamsSVM$new(hyp_list_typo))
  expect_error(HyperparamsSVM$new(hyp_list_incompatible))
  expect_error(HyperparamsSVM$new(hyp_list_no_type))




})

