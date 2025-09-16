#' @export
summary.AnalysisObject <- function(object, ...) {

  y = all.vars(object$formula)[1]

  full_data <- object$full_data

  features_names <- names(full_data)[which(names(full_data) != y)]

  cli::cat_line()
  cli::cli_h1("Summary of AnalysisObject")
  cli::cat_line()
  cli::cli_text("Stage: ", object$stage)
  cli::cli_text("Outcome Variable: ", y)
  cli::cli_text("Features: ")
  cli::cat_line()
  cli::cli_bullets(features_names)
  cli::cat_line()
  cli::cli_text("Task: ", object$task)
  cli::cli_h2("Preprocessor Steps")
  print(object$transformer$steps)

  if (object$stage == "build_model"){

    cli::cli_h2("Model Specification")
    cli::cat_line()
    cli::cli_text("Model Name: ", object$model_name)
    cli::cli_text("Model:")
    print(object$model)

  }

  if (object$stage == "fit_model"){

    cli::cli_h2("Final Model Fit")
    cli::cat_line()
    print(tune::extract_fit_parsnip(object$final_model))
    cli::cat_line()

    cli::cli_h2("Summary Results")
    print(table_evaluation_results(object))

  }

  invisible(object)

}
