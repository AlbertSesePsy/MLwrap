HyperparamsXGBoost <- R6::R6Class("XGBOOST Hyperparameters",
                             inherit = HyperparametersBase,
                             public = list(

                               mtry_tune = TRUE,
                               trees_tune = TRUE,
                               min_n_tune = TRUE,
                               tree_depth_tune = TRUE,
                               learn_rate_tune = TRUE,
                               loss_reduction_tune = TRUE,

                               default_hyperparams = function(){

                                 list(

                                   mtry = dials::mtry(range = c(3, 8)),
                                   trees = dials::trees(range = c(100, 300)),
                                   min_n = dials::min_n(range = c(5, 25)),
                                   tree_depth = dials::tree_depth(range = c(3L, 8L)),
                                   learn_rate = dials::learn_rate(range = c(-3, -1)),
                                   loss_reduction = dials::loss_reduction(range = c(-3, 1.5))


                                 )

                               },

                               check_hyperparams = function(hyperparams){

                                 valid_hparams <- c("mtry", "trees", "min_n", "tree_depth", "learn_rate", "loss_reduction", "sample_size")

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
                                     } else {

                                       def_hyperparams[[name]]

                                     }
                                   }, names(hyperparams), hyperparams)
                                 }

                                 return(def_hyperparams)

                               }
                             )
)

