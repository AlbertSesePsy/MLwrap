IntGrad_calc <- function(model, train, test, y, task, outcome_levels, use_test = FALSE){

  if (!use_test){

    test <- train

  }

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

  # Convertir modelo a float64 (double) para compatibilidad con innsight + torch 0.16.1
  torch_model$model$to(dtype = torch::torch_float64())

  # Convertir test data a numeric
  test_normalized <- as.data.frame(lapply(test, function(x) {
    if(is.numeric(x)) as.numeric(x) else x
  }))

  # Estrategia de conversión (2 intentos con dtype = "double")
  converted_model <- tryCatch({
    # Intento 1: Conversión estándar con dtype double
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")

  }, error = function(e1) {
    # Intento 2: Eval + conversión con dtype double
    torch_model$model$eval()
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")
  })

  intgrads <- innsight::run_intgrad(converted_model, data = test_normalized)

  intgrads = as.data.frame(intgrads$get_result())
  names(intgrads) = names(test)
  intgrads = dplyr::select(intgrads, names(test))

  return(intgrads)

}


IntGrad_bin <- function(model, train, test, y){

  torch_model = torch::torch_load(model$fit$model_obj)

  # Convertir modelo a float64 (double) para compatibilidad con innsight + torch 0.16.1
  torch_model$model$to(dtype = torch::torch_float64())

  # Convertir test data a numeric
  test_normalized <- as.data.frame(lapply(test, function(x) {
    if(is.numeric(x)) as.numeric(x) else x
  }))

  # Estrategia de conversión (2 intentos con dtype = "double")
  converted_model <- tryCatch({
    # Intento 1: Conversión estándar con dtype double
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")

  }, error = function(e1) {
    # Intento 2: Eval + conversión con dtype double
    torch_model$model$eval()
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")
  })

  intgrads <- innsight::run_intgrad(converted_model, data = test_normalized, verbose = F, n = 100, output_idx = 2)

  intgrads = as.data.frame(intgrads$get_result())
  names(intgrads) = names(test)
  intgrads = dplyr::select(intgrads, names(test))

  return(intgrads)

}


IntGrad_mul <- function(model, train, test, y, outcome_levels){

  y_classes = levels(train[[y]])

  torch_model = torch::torch_load(model$fit$model_obj)

  # Convertir modelo a float64 (double) para compatibilidad con innsight + torch 0.16.1
  torch_model$model$to(dtype = torch::torch_float64())

  # Convertir test data a numeric
  test_normalized <- as.data.frame(lapply(test, function(x) {
    if(is.numeric(x)) as.numeric(x) else x
  }))

  # Estrategia de conversión (2 intentos con dtype = "double")
  converted_model <- tryCatch({
    # Intento 1: Conversión estándar con dtype double
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")

  }, error = function(e1) {
    # Intento 2: Eval + conversión con dtype double
    torch_model$model$eval()
    innsight::convert(torch_model$model, input_dim = model$fit$dims$p, dtype = "double")
  })

  intgrads <- innsight::run_intgrad(converted_model, data = test_normalized, verbose = F)

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
