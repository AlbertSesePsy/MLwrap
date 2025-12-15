#' @importFrom R6 R6Class

AnalysisObject <- R6::R6Class("AnalysisObject",

                              active = list(

                                stage = function(value) {
                                  if (missing(value)) {
                                    private$.stage
                                  } else {
                                    stop("`$stage` is read only", call. = FALSE)
                                  }
                                },

                                data = function(value) {
                                  if (missing(value)) {
                                    private$.data
                                  } else {
                                    stop("`$data` is read only", call. = FALSE)
                                  }
                                },

                                transformer = function(value) {
                                  if (missing(value)) {
                                    private$.transformer
                                  } else {
                                    stop("`$transformer` is read only", call. = FALSE)
                                  }
                                },

                                task = function(value) {
                                  if (missing(value)) {
                                    private$.task
                                  } else {
                                    stop("`$task` is read only", call. = FALSE)
                                  }
                                },

                                hyperparameters = function(value) {
                                  if (missing(value)) {
                                    private$.hyperparameters
                                  } else {
                                    stop("`$hyperparameters` is read only", call. = FALSE)
                                  }
                                },

                                model = function(value) {
                                  if (missing(value)) {
                                    private$.model
                                  } else {
                                    stop("`$model` is read only", call. = FALSE)
                                  }
                                },

                                model_name = function(value) {
                                  if (missing(value)) {
                                    private$.model_name
                                  } else {
                                    stop("`$model_name` is read only", call. = FALSE)
                                  }
                                },

                                workflow= function(value) {
                                  if (missing(value)) {
                                    private$.workflow
                                  } else {
                                    stop("`$workflow` is read only", call. = FALSE)
                                  }
                                },

                                metrics = function(value) {
                                  if (missing(value)) {
                                    private$.metrics
                                  } else {
                                    stop("`$metrics` is read only", call. = FALSE)
                                  }
                                },

                                tuner = function(value) {
                                  if (missing(value)) {
                                    private$.tuner
                                  } else {
                                    stop("`$tuner` is read only", call. = FALSE)
                                  }
                                },

                                tuner_fit = function(value) {
                                  if (missing(value)) {
                                    private$.tuner_fit
                                  } else {
                                    stop("`$tuner_fit` is read only", call. = FALSE)
                                  }
                                },

                                final_model = function(value) {
                                  if (missing(value)) {
                                    private$.final_model
                                  } else {
                                    stop("`$final_model` is read only", call. = FALSE)
                                  }
                                },

                                formula = function(value) {
                                  if (missing(value)) {
                                    private$.formula
                                  } else {
                                    stop("`$formula` is read only", call. = FALSE)
                                  }
                                },

                                fit_summary = function(value) {
                                  if (missing(value)) {
                                    private$.fit_summary
                                  } else {
                                    stop("`$fit_summary` is read only", call. = FALSE)
                                  }
                                },

                                predictions = function(value){
                                  if (missing(value)) {
                                    private$.predictions
                                  } else {
                                    stop("`$predictions` is read only", call. = FALSE)
                                  }
                                },

                                outcome_levels = function(value) {
                                  if (missing(value)) {
                                    private$.outcome_levels
                                  } else {
                                    stop("`$outcome_levels` is read only", call. = FALSE)
                                  }
                                },

                                sensitivity_analysis = function(value) {
                                  if (missing(value)) {
                                    private$.sensitivity_analysis
                                  } else {
                                    stop("`$sensitivity_analysis` is read only", call. = FALSE)
                                  }
                                },

                                plots = function(value) {
                                  if (missing(value)) {
                                    private$.plots
                                  } else {
                                    stop("`$plots` is read only", call. = FALSE)
                                  }
                                },

                                tables = function(value) {
                                  if (missing(value)) {
                                    private$.tables
                                  } else {
                                    stop("`$tables` is read only", call. = FALSE)
                                  }
                                },

                                best_hyperparameters = function(value) {
                                  if (missing(value)) {
                                    private$.best_hyperparameters
                                  } else {
                                    stop("`$best_hyperparameters` is read only", call. = FALSE)
                                  }
                                },

                                dep_var = function(value){
                                  if (missing(value)){
                                    private$.dep_var
                                  } else {
                                    stop("`$dep_var` is read only", call. = FALSE)
                                  }
                                },

                                feature_names = function(value){
                                  if (missing(value)){
                                    private$.feature_names
                                  } else {
                                    stop("`$feature_names` is read only", call. = FALSE)
                                  }
                                }

                              ),

                              private = list(
                                .stage = NULL,
                                .transformer = NULL,
                                .hyperparameters = NULL,
                                .model = NULL,
                                .model_name = NULL,
                                .workflow = NULL,
                                .metrics = NULL,
                                .tuner = NULL,
                                .tuner_fit = NULL,
                                .final_model = NULL,
                                .task = NULL,
                                .formula = NULL,
                                .fit_summary = NULL,
                                .predictions = NULL,
                                .outcome_levels = NULL,
                                .sensitivity_analysis = NULL,
                                .plots = NULL,
                                .tables = NULL,
                                .best_hyperparameters = NULL,
                                .data = NULL,
                                .dep_var = NULL,
                                .feature_names = NULL,

                                add_stage = function(stage){

                                  private$.stage <- stage

                                },

                                add_formula = function(formula){

                                  private$.formula <- formula

                                },

                                add_model = function(model){

                                  private$.model <- model

                                },

                                add_model_name = function(model_name){

                                  private$.model_name <- model_name

                                },

                                add_hyperparameters = function(hyperparameters){

                                  private$.hyperparameters <- hyperparameters

                                },

                                add_workflow = function(workflow){

                                  private$.workflow <- workflow

                                },

                                add_tuner = function(tuner){

                                  private$.tuner <- tuner

                                },

                                add_metrics = function(metrics){

                                  private$.metrics <- metrics

                                },

                                add_tuner_fit = function(tuner_fit){

                                  private$.tuner_fit <- tuner_fit

                                },

                                add_final_model = function(final_model){

                                  private$.final_model = final_model

                                },

                                add_task = function(task){

                                  private$.task = task

                                },

                                add_fit_summary = function(fit_summary){

                                  private$.fit_summary = fit_summary

                                },

                                add_predictions = function(predictions){

                                  private$.predictions = predictions

                                },

                                add_outcome_levels = function(outcome_levels){

                                  private$.outcome_levels = outcome_levels

                                },

                                add_sensitivity_analysis = function(sensitivity_analysis){

                                  private$.sensitivity_analysis = sensitivity_analysis

                                },

                                add_plots = function(plot_object){

                                  private$.plots = plot_object

                                },

                                add_tables = function(table_object){

                                  private$.tables = table_object

                                },

                                add_best_hyperparameters = function(best_hyperparameters){

                                  private$.best_hyperparameters = best_hyperparameters

                                }

                              ),

                              public = list(

                                initialize = function(full_data, transformer, task ,formula, outcome_levels){

                                  private$.transformer <- transformer
                                  private$.task <- task
                                  private$.formula <- formula
                                  private$.outcome_levels <- outcome_levels
                                  private$.stage <- "preprocessing"

                                  private$.dep_var <- all.vars(formula)[1]

                                  private$.data <- DataObject$new(full_data, task, y = private$.dep_var)

                                  private$.data$preprocess(transformer)

                                  private$.feature_names <- names(private$.data$transformed$train_data)[which(names(private$.data$transformed$train_data) != private$.dep_var)]

                                },

                                modify = function(type, value) {

                                  private_names <- names(private)

                                  # Filtrar solo los atributos privados que comienzan con "."
                                  auxiliary_funcs <- base::grep("^\\.", private_names, value = TRUE)
                                  auxiliary_funcs <- base::sub("^\\.", "", auxiliary_funcs)

                                  if (!(type %in% auxiliary_funcs)){
                                    stop("Type not valid. Use one of the following types: ", paste(auxiliary_funcs), collapse = ", ")
                                  }

                                  method_name <- paste0("add_", type)  # "add_train", "add_test", "add_hyperparameters"
                                  private_method <- get(method_name, envir = private)

                                  private_method(value)
                                }
                              )
)


HyperparametersBase <- R6::R6Class("HyperparametersBase",
                                   public = list(

                                     tuning = NULL,
                                     hyperparams_constant = NULL,
                                     hyperparams_ranges = NULL,

                                     initialize = function(hyperparams = NULL){

                                       self$tuning = FALSE
                                       self$hyperparams_constant = NULL

                                       self$check_hyperparams(hyperparams)

                                       hyperparameters <- self$set_hyperparams(hyperparams)



                                       # List values are converted to dials::value_set, else a single value


                                       # Change hyperparams to dials::parameters

                                       # Convertir a objetos dials::parameters

                                       hyperparams_ranges <- Filter(function(x) inherits(x, "param"), hyperparameters)

                                       hyperparams_constant <- Filter(function(x) !inherits(x, "param"), hyperparameters)

                                       self$hyperparams_ranges <- hyperparams_ranges

                                       self$hyperparams_constant = hyperparams_constant

                                       if (length(hyperparams_ranges) > 0){self$tuning <- TRUE}

                                     },

                                     set_hyperparams = function(hyperparams){
                                       stop("Must be implemented in the subclass")
                                     },

                                     default_hyperparams = function() {
                                       stop("Must be implemented in the subclass")
                                     },

                                     check_hyperparams = function(hyperparams){
                                       stop("Must be implemented in the subclass")
                                     }

                                   )
)

DataObject <- R6::R6Class("DataObject",
                       public = list(

                         raw = list(full_data = NULL,
                                    train_data = NULL,
                                    test_data = NULL),

                         transformed = list(train_data = NULL,
                                            test_data = NULL),

                         data_id = list(train_id = NULL,
                                        test_id = NULL),

                         initialize = function(full_data, task, y = NULL){

                          self$raw$full_data <- full_data

                           if (task == "classification"){

                             train_test_split = rsample::initial_split(self$raw$full_data, prop = 0.75,
                                                                       strata = !!y)

                           }

                           else {

                             train_test_split = rsample::initial_split(self$raw$full_data, prop = 0.75)

                           }

                           self$raw$train_data <- rsample::training(train_test_split)
                           self$raw$test_data <- rsample::testing(train_test_split)

                           self$data_id$train_id <- train_test_split$in_id
                           self$data_id$test_id <- setdiff(1:nrow(self$raw$full_data), self$data_id$train_id)


                         },

                         preprocess = function(transformer){

                            train_data <- self$raw$train_data
                            test_data <- self$raw$test_data

                            rec =  transformer %>%
                              recipes::prep(training = train_data)

                            self$transformed$train_data = recipes::bake(rec, new_data = train_data)
                            self$transformed$test_data = recipes::bake(rec, new_data = test_data)

                         }

                         )



                       )




