metrics_info <- list(

  #################
  #   Regression
  #################

  rmse = c("numeric", "minimize"),
  mae = c("numeric", "minimize"),
  mpe = c("numeric", "minimize"),
  mape = c("numeric", "minimize"),
  ccc = c("numeric", "maximize"),
  smape = c("numeric", "minimize"),
  rpiq = c("numeric", "maximize"),
  rsq = c("numeric", "maximize"),

  #################
  #  Classification
  #################

  accuracy = c("class", "maximize"),
  precision = c("class", "maximize"),
  recall = c("class", "maximize"),
  bal_accuracy = c("class", "maximize"),
  specificity = c("class", "maximize"),
  sensitivity = c("class", "maximize"),
  kap = c("class", "maximize"),
  f_meas = c("class", "maximize"),
  mcc = c("class", "maximize"),
  j_index = c("class", "maximize"),
  detection_prevalence = c("class", "maximize"),

  roc_auc = c("prob", "maximize"),
  pr_auc = c("prob", "maximize"),
  gain_capture = c("prob", "maximize"),
  brier_class = c("prob", "minimize"),
  roc_aunp = c("prob", "maximize")
)

# Función para crear las funciones de métricas
create_metric_function <- function(metric_name, metric_info) {

  func_name <- base::sub("_mul$", "", metric_name)
  metric_type = metric_info[1]
  metric_direction = metric_info[2]


  #Crear la expresión de la función yardstick personalizada

  if (metric_type == "prob"){

   expr_text <- glue::glue("

     {metric_name} <- function(data, truth, estimator = NULL, na_rm = TRUE,...) {{


     yardstick::{metric_type}_metric_summarizer(
         \"{metric_name}\",
         yardstick::{func_name}_vec,
         data = data,
         truth = !!dplyr::enquo(truth),
         estimator = estimator,
         na_rm = na_rm,
         ...
       )

     }}
    ")

    } else {

    expr_text <- glue::glue("

    {metric_name} <- function(data, truth, estimate, na_rm = TRUE,...) {{
       yardstick::{metric_type}_metric_summarizer(
         \"{metric_name}\",
         yardstick::{func_name}_vec,
         data = data,
         truth = !!dplyr::enquo(truth),
         estimate = !!dplyr::enquo(estimate),
         na_rm = na_rm,
         ...
       )

     }}
   ")
    }

  pkg_env <- base::getNamespace("MLwrap")

  # Evaluar la expresión para crear la función en el entorno pkg_env
  parsed_expr <- rlang::parse_expr(expr_text)
  eval_func <- rlang::eval_tidy(parsed_expr, env = pkg_env)
  base::assign(metric_name, eval_func, envir = pkg_env)

}

# Función para convertir la función en una métrica de yardstick utilizando las funciones almacenadas en metric_funcs
convert_to_metric <- function(metrics_info) {

    pkg_env <- getNamespace("MLwrap")

    lapply(names(metrics_info), function(metric) {
    metric_name <- metric
    metric_info <- metrics_info[[metric]]
    metric_type <- metric_info[1]
    metric_direction <- metric_info[2]

    metric_func <- base::get(metric_name, envir = pkg_env)

    # Convertir la función en una métrica de yardstick usando la función almacenada
    new_metric <- base::switch(metric_type,
                         "prob" = yardstick::new_prob_metric(metric_func, metric_direction),
                         "class" = yardstick::new_class_metric(metric_func, metric_direction),
                         "numeric" = yardstick::new_numeric_metric(metric_func, metric_direction))

    # Asignar la nueva métrica al entorno

    base::assign(metric_name, new_metric, envir = pkg_env)

  })
}

# Generar todas las métricas
generate_all_metrics <- function(metrics_info) {



  # Primero crear las funciones usando eval y almacenarlas en el entorno
  base::lapply(names(metrics_info), function(m) create_metric_function(m, metrics_info[[m]]))

  # Luego convertirlas en métricas pasando metrics_info a convert_to_metric
  convert_to_metric(metrics_info)

}

generate_all_metrics(metrics_info)


