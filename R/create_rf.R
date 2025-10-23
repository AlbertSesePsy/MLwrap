create_rf <- function(hyperparams, task){

  mtry <- if (hyperparams$mtry_tune) tune::tune() else hyperparams$hyperparams_constant$mtry
  trees <- if (hyperparams$trees_tune) tune::tune() else hyperparams$hyperparams_constant$trees
  min_n <- if (hyperparams$min_n_tune) tune::tune() else hyperparams$hyperparams_constant$min_n

  model = parsnip::rand_forest(
    mtry = !!mtry,
    trees = !!trees,
    min_n = !!min_n
  ) %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode(task)

  return(model)

}
