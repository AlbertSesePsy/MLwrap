graph_nn <- function(model){

  # ----- Define Hyperparameters -----
  input_units <- model$fit$dims$p
  hidden_units <- model$fit$dims$h
  output_units <- model$fit$dims$y
  learn_rate <- signif(model$fit$parameters$learn_rate,2)
  activation <- model$fit$parameters$activation
  compact_threshold <- 8

  # ----- Start Building Graph Code -----
  graph_code <- "digraph MLP {\n  rankdir=LR;\n  splines=false;\n   ranksep=2.0;\n"

  # ----- Generate Neurons -----
  generate_neurons <- function(prefix, total_units, compact_threshold, color, apply_compact = FALSE) {
    code <- ""

    if (!apply_compact || total_units <= compact_threshold) {
      # show all neurons
      for (i in 1:total_units) {
        code <- paste0(code, "  ", prefix, i,
                       " [shape=circle, style=filled, fillcolor=", color, ", label=''];\n")
      }
    } else {
      # Input neurons
      code <- paste0(code, "  ", prefix, 1,
                     " [shape=circle, style=filled, fillcolor=", color, ", label=''];\n")
      # Add hidden layers with dots
      for (d in 1:4) {
        code <- paste0(code, "  ", prefix, "dots", d,
                       " [shape=circle, label='...', fontcolor=black, style=filled, fillcolor=white];\n")
      }
      # Last neuron
      code <- paste0(code, "  ", prefix, total_units,
                     " [shape=circle, style=filled, fillcolor=", color, ", label=''];\n")
    }
    return(code)
  }

  # Input Layer
  graph_code <- paste0(graph_code, generate_neurons("I", input_units, compact_threshold, "lightblue", apply_compact = FALSE))
  # Hidden Layer
  graph_code <- paste0(graph_code, generate_neurons("H", hidden_units, compact_threshold, "lightgreen", apply_compact = TRUE))
  # Output Layer
  graph_code <- paste0(graph_code, generate_neurons("O", output_units, compact_threshold, "lightpink", apply_compact = FALSE))

  # ----- Add Edges -----
  add_edges <- function(from_prefix, from_units, to_prefix, to_units, compact_threshold, apply_compact_from = FALSE, apply_compact_to = FALSE) {
    code <- ""

    from_nodes <- if (apply_compact_from && from_units > compact_threshold)
      c(1, paste0("dots", 1:4), from_units)
    else
      1:from_units

    to_nodes <- if (apply_compact_to && to_units > compact_threshold)
      c(1, paste0("dots", 1:4), to_units)
    else
      1:to_units

    for (i in from_nodes) {
      for (j in to_nodes) {
        code <- paste0(code, "  ", from_prefix, i, " -> ", to_prefix, j, ";\n")
      }
    }
    return(code)
  }

  # Input -> Hidden (compact only for hidden)
  graph_code <- paste0(graph_code, add_edges("I", input_units, "H", hidden_units, compact_threshold, FALSE, TRUE))
  # Hidden -> Output (compact only for hidden)
  graph_code <- paste0(graph_code, add_edges("H", hidden_units, "O", output_units, compact_threshold, TRUE, FALSE))

  # ----- Group Neurons Horizontally -----
  group_neurons <- function(prefix, units, compact_threshold, apply_compact = FALSE) {
    nodes <- if (apply_compact && units > compact_threshold)
      c(paste0(prefix, 1), paste0(prefix, "dots", 1:4), paste0(prefix, units))
    else
      paste0(prefix, 1:units)

    paste0("  {rank=same; ", paste(nodes, collapse = "; "), ";}\n")
  }

  graph_code <- paste0(graph_code, group_neurons("I", input_units, compact_threshold, apply_compact = FALSE))
  graph_code <- paste0(graph_code, group_neurons("H", hidden_units, compact_threshold, apply_compact = TRUE))
  graph_code <- paste0(graph_code, group_neurons("O", output_units, compact_threshold, apply_compact = FALSE))

  # ----- Add Hyperparameter Box -----
  hyperparams <- paste0(
    "Hyperparameters\\n",
    "Input Units: ", input_units, "\\n",
    "Hidden Units: ", hidden_units, "\\n",
    "Output Units: ", output_units, "\\n",
    "Learning Rate: ", signif(learn_rate, 3), "\\n",
    "Activation: ", activation, "\\n"
  )
#
#   graph_code <- paste0(
#     graph_code,
#     "  Hyperparams [shape=note, style=filled, fillcolor=lightyellow, fontsize=30, ",
#     "label=\"", hyperparams, "\"];\n",
#     "  Hyperparams -> I1 [style=invis, arrowhead=none];\n",
#     "  {rank=same; Hyperparams; I1;}\n"
#   )

  graph_code <- paste0(
    graph_code,
    # Nodo invisible ancla a la izquierda
    "  dummy [style=invis, width=0];\n",
    # Nodo de hiperparámetros
    "  Hyperparams [shape=note, style=filled, fillcolor=lightyellow, fontsize=30, ",
    "label=\"", hyperparams, "\"];\n",
    # Poner ambos en el nivel más a la izquierda
    "  {rank=min; dummy; Hyperparams;}\n",
    # Forzar conexión invisible (alineación izquierda → derecha)
    "  dummy -> Hyperparams [style=invis];\n"
  )


  # ----- Close Graph -----
  graph_code <- paste0(graph_code, "}")

  # ----- Render Diagram -----
  graph <- DiagrammeR::grViz(graph_code)

  return(graph)

}



