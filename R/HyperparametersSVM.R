HyperparamsSVM <- R6::R6Class("Neural Network Hyperparameters",
                             inherit = HyperparametersBase,
                             public = list(

                               cost_tune = TRUE,
                               margin_tune = TRUE,
                               rbf_sigma_tune = FALSE,
                               degree_tune = FALSE,
                               scale_factor_tune = FALSE,

                               default_hyperparams = function(){
                                 list(

                                      cost = dials::cost(range = c(-3, 3)),
                                      margin = dials::svm_margin(range = c(0, 0.2))

                                 )

                               },

                               check_hyperparams = function(hyperparams){

                                 valid_hparams_linear <- c("cost", "margin", "type")
                                 valid_hparams_rbf <- c("cost", "margin", "rbf_sigma", "type")
                                 valid_hparams_poly <- c("cost", "margin", "degree", "scale_factor", "type")

                                 if (!is.null(hyperparams)){

                                   if (hyperparams$type == "linear"){

                                      if (all(names(hyperparams) %in% valid_hparams_linear)){

                                      }

                                      else {

                                     stop(paste0("Incorrect hyperparameter list for Linear Kernel. Valid hyperparameters are: ",
                                                 paste(valid_hparams_linear, collapse = ", ")))

                                      }
                                    } else if (hyperparams$type == "rbf"){

                                      if (all(names(hyperparams) %in% valid_hparams_rbf)){

                                      }

                                      else {

                                        stop(paste0("Incorrect hyperparameter list for RBF Kernel. Valid hyperparameters are: ",
                                                    paste(valid_hparams_rbf, collapse = ", ")))

                                      }
                                    } else if (hyperparams$type == "poly"){

                                      if (all(names(hyperparams) %in% valid_hparams_poly)){

                                      }

                                      else {

                                        stop(paste0("Incorrect hyperparameter list for Polynomial Kernel. Valid hyperparameters are: ",
                                                    paste(valid_hparams_poly, collapse = ", ")))

                                      }

                                    } else {

                                      stop(paste0("Incorrect kernel type. Valid options are: 'linear', 'rbf', 'poly'."))

                                    }

                                   for (hyp_name in names(hyperparams)){

                                     hyperparam <- hyperparams[[hyp_name]]

                                     if (length(hyperparam) > 1){

                                       if (hyperparam[1] >= hyperparam[2]){

                                         print(names(hyperparams))

                                         stop(paste0("For '", hyp_name, "' lower range (", hyperparam[1],
                                                     ") is greater or equal to upper range (", hyperparam[2],")!"))

                                       }

                                     }
                                   }

                                  }
                               },

                               set_hyperparams = function(hyperparams = NULL){

                                 def_hyperparams = self$default_hyperparams()

                                 if (hyperparams$type == "rbf"){

                                   self$rbf_sigma_tune = TRUE
                                   def_hyperparams$rbf_sigma = dials::rbf_sigma(range = c(-5, 0))

                                 } else if (hyperparams$type == "poly"){

                                   self$degree_tune = TRUE
                                   self$scale_factor_tune = TRUE


                                   def_hyperparams$degree = dials::degree(range = c(1,3))
                                   def_hyperparams$scale_factor = dials::scale_factor(range = c(-5, -1))

                                 }

                                 type <- hyperparams$type

                                 hyperparams$type  <- NULL

                                 if (!is.null(hyperparams)) {

                                   def_hyperparams[names(hyperparams)] <- Map(function(name, value) {

                                     if (length(value) > 1) {

                                       if (name == "margin"){name = "svm_margin"}

                                       func <- get(name, envir = asNamespace("dials"))
                                       func(range = value)

                                     } else if (!is.null(value)){

                                       self[[paste0(name, "_tune")]] <- FALSE
                                       value
                                     } else {

                                       def_hyperparams[[name]]

                                     }
                                   }, names(hyperparams), hyperparams)
                                 }

                                 def_hyperparams$type <- type

                                 return(def_hyperparams)

                               }
                             )
                          )













