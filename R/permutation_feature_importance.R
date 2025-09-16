pfi_plot <- function(tidy_object, new_data = "test", metric = NULL){

  if (tidy_object$task == "regression"){

    pfi_reg(tidy_object, new_data = new_data, metric = metric)

  } else if (tidy_object$task == "classification"){

    pfi_bin(tidy_object, new_data = new_data, metric = metric)

  }

}


                                       ###########################
                                        #     Regression          #
                                        ###########################

pfi_calc <- function(model, train, test, y, task, metric, outcome_levels){

  if (task == "regression"){

    pfi_results <- pfi_reg(model, test, y, metric)

  } else {

    if (outcome_levels == 2){

      pfi_results <- pfi_bin(model, test, y, metric)

    }

    else{

      pfi_results <- pfi_multiclass(model, test, y, metric)

    }

  }

  return(pfi_results)

}

pfi_reg <- function(model, new_data, y, metric){

  vis <- vip::vi(model,
                 method = "permute",
                 nsim = 25,
                 metric = metric,
                 train = new_data,
                 target = y,
                 pred_wrapper = pred_reg)

  colnames(vis)[1] <- "Feature"

  return(vis)


}


                                #####################################
                                #     Binary Classification         #
                                #####################################

pfi_bin <- function(model, new_data, y, metric){

  if (metrics_info[[metric]][1] == "prob"){

    pred_func = pred_bin

  } else {

    pred_func = pred_bin_class

  }

  vis <- vip::vi(model,
                 method = "permute",
                 nsim = 25,
                 metric = metric,
                 train = new_data,
                 target = y,
                 pred_wrapper = pred_func,
                 event_level = "second")

  colnames(vis)[1] <- "Feature"

  return(vis)

}

pfi_multiclass <- function(model, new_data, y, metric){



  y_classes = levels(new_data[[y]])

  new_test <- new_data[, !(names(new_data) %in% y)]

  results = list()

  for (target_class in y_classes){

    new_y <- factor(ifelse(new_data[[y]] == target_class, 1, 0), levels = c(0,1))

    if (metrics_info[[metric]][1] == "prob"){

      predicted = paste0(".pred_", target_class)

      pred_func <- function(object, newdata){

        return(predict(object, new_data = newdata, type = "prob")[[predicted]])

      }

    }

    else{

      pred_func <- function(object, newdata){

        pred = predict(object, new_data = newdata, type = "class")$.pred_class

        bin_pred = factor(ifelse(pred == target_class, 1, 0), levels = c(0,1))

        return(bin_pred)
      }
    }

    vis <- vip::vi(model,
                                                method = "permute",
                                                nsim = 25,
                                                metric = metric,
                                                train = new_test,
                                                target = new_y,
                                                pred_wrapper = pred_func,
                                                event_level = "second")

    colnames(vis)[1] <- "Feature"

    results[[target_class]] <- vis
  }

  return(results)

}
#########################################
#     Multiclass Classification         #
#########################################



