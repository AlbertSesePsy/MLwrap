create_svm_rbf <- function(hyperparams, task){

  cost <- if (hyperparams$cost_tune) tune::tune() else hyperparams$hyperparams_constant$cost
  margin <- if (hyperparams$margin_tune) tune::tune() else hyperparams$hyperparams_constant$margin
  rbf_sigma <- if (hyperparams$rbf_sigma_tune) tune::tune() else hyperparams$hyperparams_constant$rbf_sigma

  model = parsnip::svm_rbf(
    cost = !!cost,
    margin = !!margin,
    rbf_sigma = !!rbf_sigma
  ) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode(task)

  return(model)
}

create_svm_linear <- function(hyperparams, task){

  cost <- if (hyperparams$cost_tune) tune::tune() else hyperparams$hyperparams_constant$cost
  margin <- if (hyperparams$margin_tune) tune::tune() else hyperparams$hyperparams_constant$margin

  model = parsnip::svm_linear(
    cost = !!cost,
    margin = !!margin
  ) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode(task)

  return(model)
}

create_svm_poly <- function(hyperparams, task){

  cost <- if (hyperparams$cost_tune) tune::tune() else hyperparams$hyperparams_constant$cost
  margin <- if (hyperparams$margin_tune) tune::tune() else hyperparams$hyperparams_constant$margin
  degree <- if (hyperparams$degree_tune) tune::tune() else hyperparams$hyperparams_constant$degree
  scale_factor <- if (hyperparams$scale_factor_tune) tune::tune() else hyperparams$hyperparams_constant$scale_factor

  model = parsnip::svm_poly(
    cost = !!cost,
    margin = !!margin,
    degree = !!degree,
    scale_factor = !!scale_factor
  ) %>%
    parsnip::set_engine("kernlab") %>%
    parsnip::set_mode(task)

  return(model)
}

