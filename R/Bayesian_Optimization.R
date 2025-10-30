tune_models_bayesian <- function(analysis_object, sampling_method, metrics, seed = 123, verbose = TRUE){

  bayes_control <-
    tune::control_bayes(
      no_improve    = 5L,
      time_limit    = 20,
      verbose       = verbose,
      verbose_iter  = verbose,
      save_pred     = TRUE,
      save_workflow = TRUE
    )

  extracted_hyperparams <- extract_hyperparams(analysis_object)

  tuner_object <- tryCatch(
    suppressMessages(suppressWarnings(
      tune::tune_bayes(
        object     = analysis_object$workflow,
        resamples  = sampling_method,
        iter       = 25L,
        control    = bayes_control,
        initial    = 20,
        param_info = extracted_hyperparams,
        metrics    = metrics
      )
    )),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("\\$ operator is invalid for atomic vectors", msg, fixed = TRUE)) {
        if (base::interactive()) {
          cli::cli_alert_warning("Error interno de metricas suprimido; el tuning continua.")
        }
        return(tibble::tibble(.config = NA)[0, ])
      }
      stop(e)
    }
  )

  return(tuner_object)
}

