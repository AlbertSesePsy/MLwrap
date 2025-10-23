create_nn <- function(hyperparams, task, epochs){

  hidden_units <- if (hyperparams$hidden_units_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$hidden_units)
  learn_rate <- if (hyperparams$learn_rate_tune) tune::tune() else hyperparams$hyperparams_constant$learn_rate
  activation <- if (hyperparams$activation_tune) tune::tune() else hyperparams$hyperparams_constant$activation

  model = parsnip::mlp(
    hidden_units = !!hidden_units,
    epochs = !!epochs,
    learn_rate = !!learn_rate,
    activation = !!activation
  ) %>%
    parsnip::set_engine(
      "brulee",
      optimizer = "SGD",
      stop_iter = 5,
      early_stopping = TRUE,
      batch_size = 32
    ) %>%
    parsnip::set_mode(task)

  return(model)
}
