# test_that("PFI works properly regression", {
#
#   formula = "psych_well ~ gender + age + socioec_status + depression"
#
#   hyper_nn_tune_list = list(
#     learn_rate = c(-2, -1),
#     hidden_units = c(3,10)
#   )
#
#   set.seed(123)
#
#   analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")
#
#   analysis_object <- build_model(analysis_object = analysis_object,
#                              model_name = "Neural Network",
#                              hyperparameters = hyper_nn_tune_list)
#
#   analysis_object <- fine_tuning(analysis_object = analysis_object,
#                              tuner = "Bayesian Optimization",
#                              metrics = "rmse",
#                              verbose = F)
#
#   analysis_object <- sensitivity_analysis(analysis_object, methods = c("PFI", "SHAP",
#                                                                        "Integrated Gradients", "Olden"))
#
#   pfi <- analysis_object$sensitivity_analysis$PFI
#   shap <- analysis_object$sensitivity_analysis$SHAP
#   int_grad <- analysis_object$sensitivity_analysis$IntegratedGradients
#   olden <- analysis_object$sensitivity_analysis$Olden
#
#   expect_equal(pfi$Importance[[1]], 17.70069, tolerance = 1e-1)
#   expect_equal(shap$depression[1], 18.16909, tolerance = 1e-1)
#   expect_equal(int_grad$depression[1], 0.82378044, tolerance = 1e-1)
#   expect_equal(olden$depression[1], -0.6505, tolerance = 1e-1)
#
#
# })
