HyperparamsNN <- R6::R6Class("Neural Network Hyperparameters",
                             inherit = HyperparametersBase,
                             public = list(

                               hidden_units_tune = TRUE,
                               learn_rate_tune = TRUE,
                               activation_tune = TRUE,
                               epochs = 25,

                               default_hyperparams = function() {
                                 list(learn_rate = dials::learn_rate(range = c(-3, -1)),
                                      hidden_units = dials::hidden_units(range = c(5, 20)),
                                      activation = dials::activation(values = c("relu", "tanh", "sigmoid"))
                                 )
                               },

                               check_hyperparams = function(hyperparams){

                                 valid_hparams <- c("learn_rate", "hidden_units", "activation")

                                 if (!is.null(hyperparams)){

                                   if (all(names(hyperparams) %in% valid_hparams)){

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

                                   else {

                                     stop(paste0("Incorrect hyperparameter list. Valid hyperparameters are:",

                                                 paste(valid_hparams, collapse = ",")))

                                   }

                                 }


                               },

                               set_hyperparams = function(hyperparams = NULL) {

                                 default_hyperparameters <- self$default_hyperparams()

                                 # Actualizar solo los valores proporcionados

                                 if (!is.null(hyperparams)) {

                                   if ("learn_rate" %in% names(hyperparams)) {

                                     if (length(hyperparams$learn_rate) > 1){

                                       default_hyperparameters$learn_rate <- dials::learn_rate(range = hyperparams$learn_rate)

                                     } else if (!is.null(hyperparams$learn_rate)){

                                       default_hyperparameters$learn_rate <- hyperparams$learn_rate

                                       self$learn_rate_tune = F

                                       }

                                   }

                                   if ("hidden_units" %in% names(hyperparams)) {

                                     if (length(hyperparams$hidden_units) > 1){

                                       default_hyperparameters$hidden_units <- dials::hidden_units(range = hyperparams$hidden_units)

                                     } else if (!is.null(hyperparams$hidden_units)){

                                       default_hyperparameters$hidden_units <- hyperparams$hidden_units

                                       self$hidden_units_tune = F
                                     }

                                   }

                                   if ("activation" %in% names(hyperparams)) {

                                     if (length(hyperparams$activation) > 1){

                                       default_hyperparameters$activation <- dials::activation(values = hyperparams$activation)

                                     } else if (!is.null(hyperparams$activation)){

                                       default_hyperparameters$activation <- hyperparams$activation

                                       self$activation_tune = F

                                     }

                                   }

                                 }

                                 return(default_hyperparameters)

                               }

                             )

)
