#!/usr/bin/Rscript

# Code to compare height-diameter models on a train-test split ----

#
# Se evalua la capacidad predictiva de cinco modelos h-d sobre una muestra
# de validacion (25%), tras ajustar sobre la muestra de entrenamiento (75%):
#   1. Modelo base (fase 1, fijado por especie)
#   2. Mejor extendido mixto no lineal por AIC (con Ho)        -> fase 2
#   3. Segundo mejor extendido mixto no lineal por AIC (sin Ho)-> fase 2
#   4. Modelo actual de la JCyL
#   5. Modelo general de estimacion alometrica a escala nacional (Aitor 2025)
#
# Metricas: R2, RMSE, MB (sesgo) [+ MAE auxiliar].
# Graficos: curvas h-d, observado vs predicho, residuales vs diametro
#           (heterocedasticidad) y diagramas de violin de residuales.
#
# Salidas CSV (prefijadas con sp_id, p.ej. "21_..."):
#   <sp_id>_comparativa_modelos_split.csv   -> metricas R2/RMSE/MB/MAE
#   <sp_id>_alturas_predichas_test.csv      -> h observada + pred_h1..pred_h5
#   <sp_id>_parametros_ajustados.csv        -> coeficientes (formato largo)
#   <sp_id>_parametros_ajustados_ancho.csv  -> coeficientes (formato ancho)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Load libraries ====

library(tidyverse)
library(ggpubr)
library(minpack.lm)
library(nlme)            # >>> necesario para los modelos mixtos

# Source local equations and model logic (same directory)
source("./2.0_hd_equations.r")
source("./one_model_to_rule_them_all.r")


#(*Pinus sylvestris*, 21; *Pinus nigra*, 25; *Pinus pinaster*, 26; *Quercus pyrenaica*, 43)
sp_id   <- 21
sp_name <- "Pinus sylvestris"

# Define helper functions ====

#' Perturb starting parameters slightly to assist convergence
#'
#' @param start_params List of starting parameter values.
#' @param adjustment_factor Numeric perturbation factor (default: 0.5).
#' @return A list with perturbed starting values.
adjust_start <- function(start_params, adjustment_factor = 0.5) {
  lapply(start_params, function(x) {
    x + runif(1, -adjustment_factor, adjustment_factor)
  })
}

#' Define the baseline JCyL power model function (Model 4)
#'
#' h = 1.3 + (a + b * Ho - c * dg) * exp(-d / sqrt(dbh))
#'
#' @param a,b,c,d Numeric parameters.
#' @param dbh Numeric vector representing tree diameter.
#' @param dg Quadratic mean diameter of the stand.
#' @param Ho Dominant height of the stand.
#' @return Numeric vector of predicted tree heights.
JCyL_model_base <- function(a, b, c, d, dbh, dg, Ho) {
  h <- 1.3 + (a + b * Ho - c * dg) * exp(-d / sqrt(dbh))
  return(h)
}

#' Calculate model performance metrics on test data
#'
#' @param obs Numeric vector of observed values.
#' @param pred Numeric vector of predicted values.
#' @param model_name Character string naming the model.
#' @return A dataframe with R2, RMSE, Sesgo (MB) and MAE.
calc_metrics <- function(obs, pred, model_name) {
  # coefficient of determination (R2)
  r2   <- 1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  # root mean square error (RMSE)
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  # mean bias (MB)
  bias <- mean(pred - obs, na.rm = TRUE)
  # mean absolute error (MAE) - auxiliar
  mae  <- mean(abs(obs - pred), na.rm = TRUE)
  
  data.frame(
    Modelo = model_name,
    R2 = r2,
    RMSE = rmse,
    Sesgo = bias,
    MAE = mae
  )
}

# >>> Helpers para los modelos mixtos (nlme) ===========================
# (tomados de compare_models_claude.R)

#' Localiza una columna probando varios nombres candidatos (ignora may/min)
find_col <- function(df, targets) {
  cols_lower <- tolower(names(df))
  for (t in targets) {
    hit <- names(df)[cols_lower == tolower(t)]
    if (length(hit) > 0) return(hit[1])
  }
  stop("No se encontro ninguna de [", paste(targets, collapse = ", "),
       "]. Columnas: ", paste(names(df), collapse = ", "))
}

#' "a ~ Ho; b ~ Do; c ~ Ho" -> list(a ~ Ho, b ~ Do, c ~ Ho)
parse_fixed <- function(fe_combi) {
  parts <- trimws(strsplit(fe_combi, ";")[[1]])
  lapply(parts, function(p) as.formula(trimws(p)))
}

#' "a + b ~ 1 | INVENTORY_ID" -> formula random
parse_random <- function(re_combi) {
  as.formula(trimws(re_combi))
}

#' "3.30823; 7.80406; 1.04952" + c("a","b","c") -> list(a=..., b=..., c=...)
parse_start_simple <- function(start_str, param_names) {
  vals <- as.numeric(trimws(strsplit(start_str, ";")[[1]]))
  setNames(as.list(vals), param_names)
}

#' Construye el vector start con la longitud correcta:
#' un intercepto por parametro + una pendiente (0) por cada covariable
build_start <- function(fixed_list, start_simple) {
  start_vec <- numeric(0)
  for (f in fixed_list) {
    lhs      <- all.vars(f[[2]])                          # parametro: a/b/c
    n_slopes <- length(attr(terms(f), "term.labels"))     # n covariables
    start_vec <- c(start_vec, start_simple[[lhs]], rep(0, n_slopes))
  }
  start_vec
}

#' Ajusta un modelo mixto nlme a partir de las celdas del CSV (generico)
fit_mixed_model <- function(model_name, fe_combi, re_combi, start_str, data, all_equations) {
  eq <- all_equations[[model_name]]
  if (is.null(eq)) stop("Modelo '", model_name, "' no esta en get_models_list()")
  
  param_names <- names(eq$start)
  
  fixed_list     <- parse_fixed(fe_combi)
  random_formula <- parse_random(re_combi)
  start_simple   <- parse_start_simple(start_str, param_names)
  start_vec      <- build_start(fixed_list, start_simple)
  
  model_formula <- as.formula(
    paste0("h ~ ", model_name, "(", paste(param_names, collapse = ", "), ", dbh)")
  )
  
  # do.call incrusta los valores en la llamada, en lugar de dejar
  # referencias a variables locales que 'predict' luego no encuentra
  do.call(nlme::nlme, list(
    model   = model_formula,
    data    = data,
    fixed   = fixed_list,
    random  = random_formula,
    start   = start_vec,
    control = nlme::nlmeControl(maxIter = 200, msMaxIter = 200, returnObject = TRUE)
  ))
}

#' Modelo 1: fijado a mano por especie
#' (especies 21/25 -> elmamoun_2013_M11, 26 -> pearl_1920, 43 -> gompertz_model)
get_base_model_by_species <- function(species_id) {
  if (species_id %in% c(21, 25)) {
    return("elmamoun_2013_M11")
  } else if (species_id == 26) {
    return("pearl_1920")
  } else if (species_id == 43) {
    return("gompertz_model")
  } else {
    stop("No hay modelo base definido para species_id = ", species_id)
  }
}
# >>> fin helpers nlme =================================================


# Main comparison function ====

#' Compare 5 height-diameter models on a train-test split
#'
#' @param data_path Path to the trees dataset CSV.
#' @param results_file_2 Path to the CSV of best extended models (debe incluir
#'   las columnas Modelo/model_name, AIC, fe_combi, re_combi y start).
#' @param species_id Numeric or character ID of the species (e.g., 21).
#' @param species_name Character string of the species name (e.g., "Pinus sylvestris").
#' @param output_dir Output directory for comparison files.
#' @param train_prop Numeric proportion of data for training (default: 0.75).
#' @param seed Numeric seed for reproducibility (default: 123).
#' @param split_by Particion del 75/25: "tree" (por arbol, como el preliminar)
#'   o "plot" (por parcela entera, evita fuga de informacion en los mixtos).
#' @return A list with training data, test data, fitted models, metrics and plots.
#' @export
compare_hd_models <- function(data_path = "./data/tree_data.csv",
                              results_file_2 = paste0("./data/", sp_id, "/", sp_id, "_mejores_modelos.csv"),
                              species_id   = sp_id,
                              species_name = sp_name,
                              output_dir = "./Outputs",
                              train_prop = 0.75,
                              seed = 123,
                              split_by = c("tree", "plot")) {
  
  split_by <- match.arg(split_by)
  
  # >>> prefijo de especie para todos los CSV de salida (p.ej. "21_")
  prefix <- paste0(species_id, "_")
  
  cat("Starting standalone model comparison for species ", species_name, " (ID: ", species_id, ")...\n", sep = "")
  
  # check inputs
  if (!file.exists(data_path)) {
    cat("ERROR: Trees dataset not found at ", data_path, "\n", sep = "")
    return(NULL)
  }
  
  # 1. Load and prepare dataset ----
  
  trees_raw <- read.csv(data_path)
  
  # filter data for the active species and live trees
  species_data <- trees_raw %>%
    dplyr::filter(species == species_id & dead == 0)
  
  if (nrow(species_data) == 0) {
    cat("ERROR: No data found for species ", species_id, "\n", sep = "")
    return(NULL)
  }
  
  # >>> (de claude) descarta el subinventario senescente JCyL_SEN
  species_data <- species_data %>% dplyr::filter(INVENTORY_ID != "JCyL_SEN")
  
  # >>> (de claude) BAL (Basal Area of Larger trees), por si algun modelo
  #     mixto lo usa como covariable
  species_data <- species_data %>%
    dplyr::arrange(PLOT_ID, dplyr::desc(dbh)) %>%
    dplyr::group_by(PLOT_ID) %>%
    dplyr::mutate(BAL = cumsum(g_ha) - g_ha) %>%
    dplyr::ungroup()
  
  # set seed and partition dataset into train and test splits
  set.seed(seed)
  
  if (split_by == "plot") {
    # >>> SPLIT POR PARCELA (recomendado con modelos mixtos): todos los
    #     arboles de una parcela van juntos a train O a test, nunca repartidos.
    plots       <- unique(species_data$PLOT_ID)
    n_train     <- floor(train_prop * length(plots))
    train_plots <- sample(plots, size = n_train)
    
    train_data <- species_data[species_data$PLOT_ID %in% train_plots, ]
    test_data  <- species_data[!(species_data$PLOT_ID %in% train_plots), ]
  } else {
    # SPLIT POR ARBOL (estructura del preliminar)
    train_size    <- floor(train_prop * nrow(species_data))
    train_indices <- sample(seq_len(nrow(species_data)), size = train_size)
    
    train_data <- species_data[train_indices, ]
    test_data  <- species_data[-train_indices, ]
  }
  
  cat("Data partition (", split_by, ") complete: ", nrow(train_data),
      " train rows, ", nrow(test_data), " test rows.\n", sep = "")
  
  # 2. Select models from previous results ----
  
  if (!file.exists(results_file_2)) {
    cat("ERROR: Results file not found at ", results_file_2, "\n", sep = "")
    return(NULL)
  }
  model_metrics_2 <- read.csv(results_file_2)
  if (nrow(model_metrics_2) < 2) {
    cat("ERROR: El CSV de mejores modelos necesita al menos 2 filas.\n")
    return(NULL)
  }
  
  aic_col  <- find_col(model_metrics_2, c("AIC", "aic"))
  name_col <- find_col(model_metrics_2, c("Modelo", "model_name"))
  
  # Modelo 1: NO se selecciona por AIC, esta fijado a mano por especie
  best_model_1 <- get_base_model_by_species(species_id)
  
  # Modelos 2 y 3: los dos mejores por AIC (con / sin Ho). Se guardan las
  # celdas fe_combi / re_combi / start de cada uno para reconstruir el mixto.
  rows_sorted <- model_metrics_2 %>% dplyr::arrange(.data[[aic_col]])
  row_2 <- rows_sorted %>% dplyr::slice(1)
  row_3 <- rows_sorted %>% dplyr::slice(2)
  
  best_model_2 <- row_2 %>% dplyr::pull(.data[[name_col]])
  fe_2 <- row_2 %>% dplyr::pull(fe_combi)
  re_2 <- row_2 %>% dplyr::pull(re_combi)
  st_2 <- row_2 %>% dplyr::pull(start)
  
  best_model_3 <- row_3 %>% dplyr::pull(.data[[name_col]])
  fe_3 <- row_3 %>% dplyr::pull(fe_combi)
  re_3 <- row_3 %>% dplyr::pull(re_combi)
  st_3 <- row_3 %>% dplyr::pull(start)
  
  cat("Models selected for fitting: \n")
  cat("  1. ", best_model_1, " (fijado por especie)\n", sep = "")
  cat("  2. ", best_model_2, " | fe: ", fe_2, " | re: ", re_2, "\n", sep = "")
  cat("  3. ", best_model_3, " | fe: ", fe_3, " | re: ", re_3, "\n", sep = "")
  
  # retrieve equations from the compiled list
  all_equations <- get_models_list()
  
  # 3. Fit Model 1 on training split ----
  
  cat("Fitting Local Model 1 (", best_model_1, ") on training split...\n", sep = "")
  
  model_func_1  <- all_equations[[best_model_1]]$func
  start_params_1 <- all_equations[[best_model_1]]$start
  param_names_1  <- names(start_params_1)
  
  # build formula dynamically
  formula_str_1 <- paste0("h ~ model_func_1(", paste(param_names_1, collapse = ", "), ", dbh)")
  formula_1 <- as.formula(formula_str_1)
  
  fit_1 <- tryCatch({
    minpack.lm::nlsLM(formula_1, data = train_data, start = start_params_1)
  }, error = function(e) {
    attempt <- 1
    fit_success <- FALSE
    while (attempt <= 10 && !fit_success) {
      adjusted_start <- adjust_start(start_params_1)
      fit_try <- tryCatch({
        minpack.lm::nlsLM(formula_1, data = train_data, start = adjusted_start)
      }, error = function(err) NULL)
      if (!is.null(fit_try)) {
        fit_success <- TRUE
        return(fit_try)
      }
      attempt <- attempt + 1
    }
    return(NULL)
  })
  
  if (is.null(fit_1)) {
    cat("ERROR: Failed to fit Local Model 1 on training split.\n")
    return(NULL)
  }
  
  # 4. Fit Model 2 (mixto nlme) on training split ----   >>>
  
  cat("Fitting Local Model 2 (", best_model_2, ") como modelo mixto nlme...\n", sep = "")
  fit_2 <- tryCatch(
    fit_mixed_model(best_model_2, fe_2, re_2, st_2, train_data, all_equations),
    error = function(e) { cat("ERROR fit_2 (nlme): ", conditionMessage(e), "\n"); NULL })
  if (is.null(fit_2)) {
    cat("ERROR: Failed to fit Local Model 2 on training split.\n")
    return(NULL)
  }
  
  # 4.1. Fit Model 3 (mixto nlme) on training split ----   >>>
  
  cat("Fitting Local Model 3 (", best_model_3, ") como modelo mixto nlme...\n", sep = "")
  fit_3 <- tryCatch(
    fit_mixed_model(best_model_3, fe_3, re_3, st_3, train_data, all_equations),
    error = function(e) { cat("ERROR fit_3 (nlme): ", conditionMessage(e), "\n"); NULL })
  if (is.null(fit_3)) {
    cat("ERROR: Failed to fit Local Model 3 on training split.\n")
    return(NULL)
  }
  
  # 5. Fit Model 4 (Baseline JCyL) on training split ----
  
  cat("Fitting Baseline Model 4 (JCyL) on training split...\n")
  fit_4 <- tryCatch({
    minpack.lm::nlsLM(h ~ JCyL_model_base(a, b, c, d, dbh, dg, Ho), data = train_data,
                      start = list(a = 3.35, b = 1.04, c = 0.10, d = 6.02))
  }, error = function(e) {
    cat("ERROR: Failed to fit Baseline Model 4: ", conditionMessage(e), "\n")
    return(NULL)
  })
  if (is.null(fit_4)) {
    cat("ERROR: Failed to fit Baseline Model 4.\n")
    return(NULL)
  }
  
  # 6. Predict on test dataset ----
  
  cat("Running predictions on test split...\n")
  
  # predict local model heights
  test_data$pred_h1 <- stats::predict(fit_1, newdata = test_data)
  # >>> level = 0 (prediccion poblacional): iguala el terreno con los modelos
  #     locales, que no tienen efecto aleatorio de parcela. Usar level=1 daria
  #     ventaja injusta a los mixtos en la comparacion.
  test_data$pred_h2 <- stats::predict(fit_2, newdata = test_data, level = 0)
  test_data$pred_h3 <- stats::predict(fit_3, newdata = test_data, level = 0)
  test_data$pred_h4 <- stats::predict(fit_4, newdata = test_data)
  
  # predict general model (Aitor 2025) using pre-calculated parameters
  test_data_general <- predict_height(
    df = test_data,
    path_to_pars = "./data/",
    dbh_col = "dbh",
    species_value = "code",
    species_col = "species"
  )
  test_data$pred_h5 <- test_data_general$pred_h
  
  # 7. Calculate evaluation metrics on test split ----
  
  metrics_1 <- calc_metrics(test_data$h, test_data$pred_h1, paste0("Local 1: ", best_model_1))
  metrics_2 <- calc_metrics(test_data$h, test_data$pred_h2, paste0("Local 2: ", best_model_2))
  metrics_3 <- calc_metrics(test_data$h, test_data$pred_h3, paste0("Local 3: ", best_model_3))
  metrics_4 <- calc_metrics(test_data$h, test_data$pred_h4, "Referencia: JCyL")
  metrics_5 <- calc_metrics(test_data$h, test_data$pred_h5, "General: Aitor 2025")
  
  comparison_table <- dplyr::bind_rows(metrics_1, metrics_2, metrics_3, metrics_4, metrics_5)
  
  # ensure output folders exist
  comparativa_dir <- file.path(output_dir, "Graficos_Comparativa")
  dir.create(comparativa_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- 7a. Metricas (tabla limpia, una fila por modelo) ---
  csv_output_path <- file.path(output_dir, paste0(prefix, "comparativa_modelos_split.csv"))
  write.csv(comparison_table, csv_output_path, row.names = FALSE)
  cat("Metrics table saved successfully at: ", csv_output_path, "\n", sep = "")
  
  # --- 7b. Alturas predichas de los 5 modelos (muestra de validacion) ---
  id_cols <- intersect(c("TREE_ID", "PLOT_ID", "INVENTORY_ID"), names(test_data))
  pred_export <- test_data[, c(id_cols, "dbh", "h",
                               "pred_h1", "pred_h2", "pred_h3", "pred_h4", "pred_h5")]
  names(pred_export)[names(pred_export) == "h"] <- "h_obs"
  pred_path <- file.path(output_dir, paste0(prefix, "alturas_predichas_test.csv"))
  write.csv(pred_export, pred_path, row.names = FALSE)
  cat("Alturas predichas guardadas en: ", pred_path, "\n", sep = "")
  
  # --- 7c. Parametros AJUSTADOS en tabla aparte (formato largo + ancho) ---
  # nlsLM -> coef(); nlme -> efectos fijos via fixef()
  get_fitted <- function(fit, modelo) {
    cf <- tryCatch(
      if (inherits(fit, "nlme")) nlme::fixef(fit) else stats::coef(fit),
      error = function(e) numeric(0)
    )
    if (length(cf) == 0) return(NULL)
    data.frame(Modelo = modelo,
               Parametro = names(cf),
               valor_ajustado = as.numeric(cf),
               row.names = NULL)
  }
  
  params_ajustados <- dplyr::bind_rows(
    get_fitted(fit_1, paste0("Local 1: ", best_model_1)),
    get_fitted(fit_2, paste0("Local 2: ", best_model_2)),
    get_fitted(fit_3, paste0("Local 3: ", best_model_3)),
    get_fitted(fit_4, "Referencia: JCyL")
    # Modelo 5 (General: Aitor 2025) no se reajusta aqui -> coeficientes en ./data/
  )
  
  params_path <- file.path(output_dir, paste0(prefix, "parametros_ajustados.csv"))
  write.csv(params_ajustados, params_path, row.names = FALSE)
  cat("Parametros ajustados guardados en: ", params_path, "\n", sep = "")
  
  # version ancha (un modelo por fila, un parametro por columna) para la memoria
  params_ancho <- tidyr::pivot_wider(params_ajustados,
                                     names_from = Parametro,
                                     values_from = valor_ajustado)
  params_ancho_path <- file.path(output_dir, paste0(prefix, "parametros_ajustados_ancho.csv"))
  write.csv(params_ancho, params_ancho_path, row.names = FALSE)
  cat("Parametros ajustados (ancho) guardados en: ", params_ancho_path, "\n", sep = "")
  
  # 8. Generate diagnostic comparison plots in Spanish ----
  
  cat("Generating comparison plots...\n")
  
  # plot 1: superimpose curves on the test scatter plot
  dbh_seq <- seq(min(test_data$dbh, na.rm = TRUE), max(test_data$dbh, na.rm = TRUE), length.out = 1000)
  
  # >>> Los modelos 2/3/4 dependen de covariables de rodal (Ho, Do, dg, N, G,
  #     BAL). No se pueden evaluar solo con dbh, asi que se fijan a su media en
  #     train: cada curva representa el arbol "promedio" de un rodal medio.
  seq_cov <- data.frame(
    dbh = dbh_seq,
    Ho  = mean(train_data$Ho,  na.rm = TRUE),
    Do  = mean(train_data$Do,  na.rm = TRUE),
    dg  = mean(train_data$dg,  na.rm = TRUE),
    N   = mean(train_data$N,   na.rm = TRUE),
    G   = mean(train_data$G,   na.rm = TRUE),
    BAL = mean(train_data$BAL, na.rm = TRUE)
  )
  
  # predictions for sequence
  seq_pred_1 <- stats::predict(fit_1, newdata = data.frame(dbh = dbh_seq))
  seq_pred_2 <- stats::predict(fit_2, newdata = seq_cov, level = 0)   # >>>
  seq_pred_3 <- stats::predict(fit_3, newdata = seq_cov, level = 0)   # >>>
  seq_pred_4 <- stats::predict(fit_4, newdata = seq_cov)             # >>> usa dg + Ho
  
  # predictions for sequence (general model)
  seq_df <- data.frame(dbh = dbh_seq, species = species_id)
  seq_df_general <- predict_height(
    df = seq_df,
    path_to_pars = "./data/",
    dbh_col = "dbh",
    species_value = "code",
    species_col = "species"
  )
  seq_pred_5 <- seq_df_general$pred_h
  
  curves_data <- data.frame(
    dbh = rep(dbh_seq, 5),
    h_pred = c(seq_pred_1, seq_pred_2, seq_pred_3, seq_pred_4, seq_pred_5),
    Modelo = rep(c(
      paste0("Local 1: ", best_model_1),
      paste0("Local 2: ", best_model_2),
      paste0("Local 3: ", best_model_3),
      "Referencia: JCyL",
      "General: Aitor 2025"
    ), each = length(dbh_seq))
  )
  
  plot_curves <- ggplot2::ggplot() +
    ggplot2::geom_point(data = test_data, ggplot2::aes(x = dbh, y = h), color = "gray60", alpha = 0.5, size = 1.5) +
    ggplot2::geom_line(data = curves_data, ggplot2::aes(x = dbh, y = h_pred, color = Modelo), linewidth = 1.1) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Comparacion de curvas h-d sobre datos de test",
      subtitle = paste0(species_name, " (25% datos de test)"),
      x = "Diametro (cm)",
      y = "Altura (m)",
      color = "Modelo"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      legend.position = "bottom"
    )
  
  # plot 2: observed vs predicted heights panel plot
  pred_long <- test_data %>%
    dplyr::select(dbh, h, pred_h1, pred_h2, pred_h3, pred_h4, pred_h5) %>%
    tidyr::pivot_longer(
      cols = c(pred_h1, pred_h2, pred_h3, pred_h4, pred_h5),
      names_to = "model_id",
      values_to = "h_pred"
    ) %>%
    dplyr::mutate(
      Modelo = dplyr::case_when(
        model_id == "pred_h1" ~ paste0("Local 1: ", best_model_1),
        model_id == "pred_h2" ~ paste0("Local 2: ", best_model_2),
        model_id == "pred_h3" ~ paste0("Local 3: ", best_model_3),
        model_id == "pred_h4" ~ "Referencia: JCyL",
        model_id == "pred_h5" ~ "General: Aitor 2025"
      )
    )
  
  plot_obs_pred <- ggplot2::ggplot(pred_long, ggplot2::aes(x = h_pred, y = h)) +
    ggplot2::geom_point(color = "forestgreen", alpha = 0.4, size = 1.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1.1) +
    ggplot2::facet_wrap(~Modelo, ncol = 2) +
    ggplot2::labs(
      title = "Valores observados vs. predichos por modelo",
      subtitle = paste0(species_name, " (25% datos de test)"),
      x = "Altura predicha (m)",
      y = "Altura observada (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # plot 3: residuals vs diameter (heterocedasticidad) ----   >>> (de claude)
  residuals_data <- pred_long %>%
    dplyr::mutate(residuals = h_pred - h)
  
  plot_res_dbh <- ggplot2::ggplot(residuals_data, ggplot2::aes(x = dbh, y = residuals)) +
    ggplot2::geom_point(color = "forestgreen", alpha = 0.3, size = 1.2) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
    ggplot2::facet_wrap(~Modelo, ncol = 2) +
    ggplot2::labs(
      title = "Residuales frente al diametro por modelo",
      subtitle = paste0(species_name, " - patron ideal: nube plana centrada en 0"),
      x = "Diametro (dbh, cm)",
      y = "Residual (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # plot 4: violin and boxplot of residuals by model
  plot_violin_res <- ggplot2::ggplot(residuals_data, ggplot2::aes(x = Modelo, y = residuals, fill = Modelo)) +
    ggplot2::geom_violin(alpha = 0.6, trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.15, fill = "white", outlier.alpha = 0.3, alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1.1) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Distribucion de residuales por modelo (Violin)",
      subtitle = paste0(species_name, " (25% datos de test)"),
      x = "Modelo",
      y = "Residuales (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1)
    )
  
  # save plots (tambien prefijados con sp_id, por coherencia)
  ggplot2::ggsave(file.path(comparativa_dir, paste0(prefix, "comparativa_curvas_hd.png")), plot_curves, dpi = 300, width = 8, height = 6)
  ggplot2::ggsave(file.path(comparativa_dir, paste0(prefix, "comparativa_obs_vs_pred.png")), plot_obs_pred, dpi = 300, width = 10, height = 8)
  ggplot2::ggsave(file.path(comparativa_dir, paste0(prefix, "residuales_vs_dbh.png")), plot_res_dbh, dpi = 300, width = 10, height = 8)
  ggplot2::ggsave(file.path(comparativa_dir, paste0(prefix, "comparativa_violin_residuales.png")), plot_violin_res, dpi = 300, width = 8, height = 6)
  
  cat("All comparison plots saved successfully in: ", comparativa_dir, "\n", sep = "")
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    fit_1 = fit_1,
    fit_2 = fit_2,
    fit_3 = fit_3,
    fit_4 = fit_4,
    metrics = comparison_table,
    params = params_ajustados,
    plot_curves = plot_curves,
    plot_obs_pred = plot_obs_pred,
    plot_res_dbh = plot_res_dbh,
    plot_violin_res = plot_violin_res
  ))
}

# Example of standalone run ====

res <- compare_hd_models(
  data_path = "./data/tree_data.csv",
  results_file_2 = paste0("./data/", sp_id, "/", sp_id, "_mejores_modelos.csv"),
  species_id   = sp_id,
  species_name = sp_name,
  output_dir = "./Outputs",
  split_by = "tree"   # cambia a "plot" para particion por parcela (ver nota)
)
