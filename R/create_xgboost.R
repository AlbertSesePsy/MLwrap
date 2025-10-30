create_xgboost <- function(hyperparams, task){

  mtry <- if (hyperparams$mtry_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$mtry)
  trees <- if (hyperparams$trees_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$trees)
  min_n <- if (hyperparams$min_n_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$min_n)
  tree_depth <- if (hyperparams$tree_depth_tune) tune::tune() else as.integer(hyperparams$hyperparams_constant$tree_depth)
  learn_rate <- if (hyperparams$learn_rate_tune) tune::tune() else as.numeric(hyperparams$hyperparams_constant$learn_rate)
  loss_reduction <- if (hyperparams$loss_reduction_tune) tune::tune() else as.numeric(hyperparams$hyperparams_constant$loss_reduction)

  model = parsnip::boost_tree(

    mtry = !!mtry,
    trees = !!trees,
    min_n = !!min_n,
    tree_depth = !!tree_depth,
    learn_rate = !!learn_rate,
    loss_reduction = !!loss_reduction

  ) %>%
    parsnip::set_engine("xgboost") %>%
    parsnip::set_mode(task)

  return(model)
}
