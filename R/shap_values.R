shap_calc <- function(model, train, test, y, task, outcome_levels){

  if (task == "regression"){

    shap_reg(model, train, test, y)

  } else if (task == "classification"){

    if (outcome_levels == 2){

      shap_bin(model, train, test, y)

    } else {

      shap_mul(model, train, test, y)

    }

  }

}



###########################
#     Regression          #
###########################

shap_reg <- function(model, train, test, y){

  y_vals = train[[y]]

  train <- train[which(names(train) != y)]
  test <- test[which(names(test) != y)]

  # shap_vals = shapr::explain(model, phi0 = mean(y_vals),
  #                 approach = "empirical",
  #                 x_train = train,
  #                 x_explain = test,
  #                 predict_model = pred_reg,
  #                 max_n_coalitions = 40,
  #                 n_MC_samples = 1e2,
  #                 iterative = T,
  #                 seed = 123)
  #
  # shap_vals = shap_vals$shapley_values_est %>% dplyr::select(names(train))

  shap_vals <- fastshap::explain(model, X = as.data.frame(train),
                                 pred_wrapper = pred_reg, nsim = 50, adjust = T,
                                 newdata = as.data.frame(test))

  shap_vals <- as.data.frame(shap_vals)

  return(shap_vals)

}

##################################
#     Binary Classification      #
##################################

shap_bin <- function(model, train, test, y){

  target_class = levels(train[[y]])[1]

  y_vals = factor(ifelse(train[[y]] == target_class, 1, 0), levels = c(0,1))

  phi0 = mean(y_vals == levels(y_vals)[1])

  train <- train[which(names(train) != y)]
  test <- test[which(names(test) != y)]

  # shap_vals = shapr::explain(model, phi0 = phi0,
  #                            approach = "empirical",
  #                            x_train = train,
  #                            x_explain = test,
  #                            predict_model = pred_bin,
  #                            max_n_coalitions = 40,
  #                            n_MC_samples = 1e3,
  #                            iterative = T,
  #                            verbose = NULL,
  #                            seed = 123)
  #
  # shap_vals = shap_vals$shapley_values_est %>% dplyr::select(names(train))

  shap_vals <- fastshap::explain(model, X = as.data.frame(train),
                                 pred_wrapper = pred_bin, nsim = 50, adjust = T,
                                 newdata = as.data.frame(test))

  shap_vals <- as.data.frame(shap_vals)

  return(shap_vals)

}

######################################
#     Multiclass Classification      #
######################################

shap_mul <- function(model, train, test, y){

  results = list()

  y_classes = levels(train[[y]])

  new_train <- train[which(names(train) != y)]
  new_test <- test[which(names(test) != y)]

  for (target_class in y_classes){

    y_vals = factor(ifelse(test[[y]] == target_class, 1, 0), levels = c(0,1))

    pred_class = paste0(".pred_", target_class)

    phi0 = mean(y_vals == 1)

    pred_func <- function(object, newdata){return(predict(model, newdata, type = "prob")[[pred_class]])}

    # shap_vals = shapr::explain(model, phi0 = phi0,
    #                          approach = "empirical",
    #                          x_train = new_train,
    #                          x_explain = new_test,
    #                          predict_model = pred_func,
    #                          max_n_coalitions = 40,
    #                          n_MC_samples = 1e3,
    #                          iterative = T,
    #                          seed = 123)
    #
    # shap_vals = shap_vals$shapley_values_est %>% dplyr::select(names(new_train))
    #
    # results[[target_class]] <- shap_vals

    shap_vals <- fastshap::explain(model, X = as.data.frame(new_train),
                                   pred_wrapper = pred_func, nsim = 50, adjust = T,
                                   newdata = as.data.frame(new_test))

    results[[target_class]] <- as.data.frame(shap_vals)


  }

  return(results)

}

