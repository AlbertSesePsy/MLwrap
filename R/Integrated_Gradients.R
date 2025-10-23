IntGrad_calc <- function(model, train, test, y, task, outcome_levels){

    test <- test[which(names(test) != y)]

    if (outcome_levels > 2){

      intgrads <- IntGrad_mul(model, train, test, y, outcome_levels)

    } else if (outcome_levels ==2){

      intgrads <- IntGrad_bin(model, train, test, y)

    } else {

      intgrads <- IntGrad_reg(model, train, test, y)

    }

    return(intgrads)

}

IntGrad_reg <- function(model, train, test, y){

  torch_model = torch::torch_load(model$fit$model_obj)

  converted_model <-
    innsight::convert(torch_model$model,
                      input_dim = model$fit$dims$p
    )

  intgrads <- innsight::run_intgrad(converted_model, data = test)

  intgrads = as.data.frame(intgrads$get_result())

  names(intgrads) = names(test)

  intgrads = dplyr::select(intgrads, names(test))

  return(intgrads)

}

IntGrad_bin <- function(model, train, test, y){

    torch_model = torch::torch_load(model$fit$model_obj)

    converted_model <-
      innsight::convert(torch_model$model,
              input_dim = model$fit$dims$p
      )

    intgrads <- innsight::run_intgrad(converted_model, data = test, verbose = F, n = 100,
                                      output_idx = 2)

    intgrads = as.data.frame(intgrads$get_result())

    #idx <- seq(ncol(test) + 1, ncol(test) * 2)

    #∫intgrads <- as.data.frame(intgrads[idx])

    names(intgrads) = names(test)

    intgrads = dplyr::select(intgrads, names(test))

    return(intgrads)

}

IntGrad_mul <- function(model, train, test, y, outcome_levels){

  y_classes = levels(train[[y]])

  torch_model = torch::torch_load(model$fit$model_obj)

  converted_model <-
    innsight::convert(torch_model$model,
                      input_dim = model$fit$dims$p
    )

  intgrads <- innsight::run_intgrad(converted_model, data = test, verbose = F)

  intgrads = as.data.frame(intgrads$get_result())

  result = list()

  for (i in 1:outcome_levels){

    idx <- seq((i-1)*ncol(test) + 1, ncol(test) * i)

    sub_result <- as.data.frame(intgrads[idx])

    names(sub_result) <- names(test)

    result[[y_classes[i]]] <- sub_result

  }

  return(result)

}





