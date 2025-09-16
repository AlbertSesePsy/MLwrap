HyperparamsRF <- R6::R6Class("Random Forest Hyperparameters",

                              inherit = HyperparametersBase,

                              public = list(

                                mtry_tune = TRUE,
                                trees_tune = TRUE,
                                min_n_tune = TRUE,

                                default_hyperparams = function(){

                                  list(

                                    mtry = dials::mtry(range = c(3, 8)),
                                    trees = dials::trees(range = c(100, 300)),
                                    min_n = dials::min_n(range = c(2, 25))

                                  )

                                },

                                check_hyperparams = function(hyperparams){

                                  valid_hparams <- c("mtry", "trees", "min_n")

                                  if (!is.null(hyperparams)){

                                    if (all(names(hyperparams) %in% valid_hparams)){

                                    }

                                    else {

                                      stop(paste0("Incorrect hyperparameter list. Valid hyperparameters are:",
                                                  paste(valid_hparams, collapse = ",")))

                                    }
                                  }
                                },

                                set_hyperparams = function(hyperparams = NULL){

                                  def_hyperparams = self$default_hyperparams()

                                    if (!is.null(hyperparams)) {

                                      def_hyperparams[names(hyperparams)] <- Map(function(name, value) {

                                        if (length(value) > 1) {

                                          func <- get(name, envir = asNamespace("dials"))
                                          func(range = value)

                                        } else if (!is.null(value)){

                                          self[[paste0(name, "_tune")]] <- FALSE
                                          value
                                        } else{

                                          def_hyperparams[[name]]

                                        }
                                      }, names(hyperparams), hyperparams)
                                    }

                                  return(def_hyperparams)

                                }
                              )
)
