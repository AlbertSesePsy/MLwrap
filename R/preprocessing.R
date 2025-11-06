subset_data <- function(formula, data){

  if (all.vars(formula)[2] != "."){
    data = data[all.vars(formula)]
  }

  return(data)
}

standarize_predictors <- function(rec, norm_num_vars){

  if (any(norm_num_vars == "all")){

    rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())

  } else{

    rec <- recipes::step_normalize(rec, all_of(norm_num_vars))

  }

  return(rec)

}

one_hot_predictors <- function(rec, encode_cat_vars, one_hot = T){

  if (any(encode_cat_vars == "all")){

    rec <- recipes::step_dummy(rec, recipes::all_factor_predictors(), one_hot = one_hot)

  } else{

    rec <- recipes::step_dummy(rec, all_of(encode_cat_vars), one_hot = one_hot)

  }

  return(rec)

}

#' Preprocessing Data Matrix
#'
#' @description
#'
#' The **preprocessing()** function streamlines data preparation for regression
#' and classification tasks by integrating variable selection, type conversion,
#' normalization, and categorical encoding into a single workflow. It takes a
#' data frame and a formula, applies user-specified transformations to numeric
#' and categorical variables using the recipes package, and ensures the outcome
#' variable is properly formatted. The function returns an AnalysisObject
#' containing both the processed data and the transformation pipeline,
#' supporting reproducible and efficient modeling (Kuhn & Wickham, 2020).
#'
#' @param df Input DataFrame. Either a data.frame or tibble.
#' @param formula Modelling Formula. A string of characters or formula.
#' @param task Modelling Task. Either "regression" or "classification".
#' @param num_vars Optional vector of names of the numerical features.
#' @param cat_vars Optional vector of names of the categorical features.
#' @param encode_cat_vars One Hot Encode Categorical Features. Either vector of
#'  names of categorical features to be encoded or "all" (default).
#' @param norm_num_vars Normalize numeric features as z-scores. Either vector
#'  of names of numerical features to be normalized or "all" (default).
#' @param y_levels Optional ordered vector with names of the target variable
#'  levels (Classification task only).
#' @returns The object returned by the preprocessing function encapsulates a
#' dataset specifically prepared for ML analysis. This object contains the
#' preprocessed data—where variables have been selected, standardized, encoded,
#' and formatted according to the requirements of the chosen modeling task
#' (regression or classification) —as well as a recipes::recipe object that
#' documents all preprocessing steps applied. By automating essential
#' transformations such as normalization, one-hot encoding of categorical
#' variables, and the handling of missing values, the function ensures the data
#' is optimally structured for input into machine learning algorithms. This
#' comprehensive preprocessing not only exposes the underlying structure of the
#' data and reduces the risk of errors, but also provides a robust foundation
#' for subsequent modeling, validation, and interpretation within the machine
#' learning workflow (Kuhn & Johnson, 2019).
#' @examples
#' # Example 1: Dataset with preformatted categorical variables
#' # In this case, internal options for variable types are not needed since
#' # categorical features are already formatted as factors.
#'
#' library(MLwrap)
#'
#' data(sim_data) # sim_data is a simulated dataset with psychological variables
#'
#' wrap_object <- preprocessing(
#'           df = sim_data,
#'           formula = psych_well ~ depression + emot_intel + resilience + life_sat + gender,
#'           task = "regression"
#'          )
#'
#' # Example 2: Dataset where neither the outcome nor the categorical features
#' # are formatted as factors and all categorical variables are specified to be
#' # formatted as factors
#'
#' wrap_object <- preprocessing(
#'            df = sim_data,
#'            formula = psych_well_bin ~ gender + depression + age + life_sat,
#'            task = "classification",
#'            cat_vars = c("gender")
#'          )
#' @references
#' Kuhn, M., & Johnson, K. (2019). *Feature Engineering and Selection: A
#' Practical Approach for Predictive Models*. Chapman and Hall/CRC.
#' \doi{10.1201/9781315108230}
#'
#' Kuhn, M., & Wickham, H. (2020). *Tidymodels: a collection of packages for
#' modeling and machine learning using tidyverse principles*.
#' \url{https://www.tidymodels.org}.
#' @export
preprocessing <- function(df, formula, task = "regression", num_vars = NULL, cat_vars = NULL,
                          norm_num_vars = "all", encode_cat_vars = "all", y_levels = NULL){

          formula = as.formula(formula)

          check_args_preprocessing(df = df, formula = formula, task = task, num_vars = num_vars,
                                   cat_vars = cat_vars, norm_num_vars = norm_num_vars,
                                   encode_cat_vars = encode_cat_vars)

          # Subset data from formula

          df <- subset_data(formula = formula, data = df)

          outcome_levels = 0

          if (task == "classification"){

            y = all.vars(formula)[1]

            if (!is.null(y_levels)){

              if (any(!(y_levels %in% levels(as.factor(df[[y]]))))){

                stop("y_levels do not correspond to original levels")

              }

              df[[y]] <- factor(df[[y]], levels = y_levels)

            } else{

                df[[y]] = as.factor(df[[y]])

            }

            outcome_levels = length(levels(df[[y]]))

          }

          # Create recipe

          rec = recipes::recipe(formula = formula, data = df)


          # Check numerical variables are numeric

          if (!is.null(num_vars)) {

              rec <- rec %>% recipes::step_mutate_at(all_of(num_vars),
                                            fn = as.numeric)
          }

          # Check categorical variables are factor

          if (!is.null(cat_vars)) {

              rec <- rec %>% recipes::step_mutate_at(all_of(cat_vars),
                                            fn = as.factor)
          }

          # Normalize selected numerical columns

          if (!is.null(norm_num_vars)) {

            rec <- standarize_predictors(rec = rec, norm_num_vars = norm_num_vars)

          }

          # Encode selected categorical columns

          if (!is.null(encode_cat_vars)){

              rec <- one_hot_predictors(rec = rec, encode_cat_vars = encode_cat_vars)

          }

          # Create AnalysisObject with data and recipe

          analysis_object <- AnalysisObject$new(full_data = df, transformer = rec, task = task,
                                          formula = formula, outcome_levels = outcome_levels)

          return(analysis_object)

}


