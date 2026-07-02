#!/usr/bin/Rscript

# Code to compare height-diameter models on a train-test split ----
#
# Aitor Vázquez Veloso
# 2026-06-17
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Load libraries ====

library(tidyverse)
library(ggpubr)
library(minpack.lm)

# Source local equations and model logic (same directory)
source("./2.0_hd_equations.r")
source("./one_model_to_rule_them_all.r")

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

#' Define the baseline power model function (Model 4)
#'
#' @param a Numeric parameter.
#' @param b Numeric parameter.
#' @param dbh Numeric vector representing tree diameter.
#' @return Numeric vector of predicted tree heights.
power_model_base <- function(a, b, dbh) {
  h <- 1.3 + a * dbh^b
  return(h)
}

#' Calculate model performance metrics on test data
#'
#' @param obs Numeric vector of observed values.
#' @param pred Numeric vector of predicted values.
#' @param model_name Character string naming the model.
#' @return A dataframe with calculated metrics.
calc_metrics <- function(obs, pred, model_name) {
  # coefficient of determination (R2)
  r2 <- 1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  # root mean square error (RMSE)
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  # mean bias
  bias <- mean(pred - obs, na.rm = TRUE)
  # mean absolute error (MAE)
  mae <- mean(abs(obs - pred), na.rm = TRUE)
  
  data.frame(
    Modelo = model_name,
    R2 = r2,
    RMSE = rmse,
    Sesgo = bias,
    MAE = mae
  )
}

# Main comparison function ====

#' Compare 4 height-diameter models on a train-test split
#'
#' @param data_path Path to the trees dataset CSV.
#' @param results_file Path to previous results CSV to select top models.
#' @param results_file_2 Path to second results CSV to select the top model 2.
#' @param species_id Numeric or character ID of the species (e.g., 21).
#' @param species_name Character string of the species name (e.g., "Pinus sylvestris").
#' @param output_dir Output directory for comparison files.
#' @param train_prop Numeric proportion of data for training (default: 0.75).
#' @param seed Numeric seed for reproducibility (default: 123).
#' @return A list with training data, test data, fitted models, and metrics table.
#' @export
compare_hd_models <- function(data_path = "./data/1_harmonized_tree_df_jcyl_sfni.csv",
                              results_file = "./data/21/Modelos95_resultados_modelos.csv",
                              results_file_2 = "./data/21/Modelos95_resultados_modelos.csv",
                              species_id = 21,
                              species_name = "Pinus sylvestris",
                              output_dir = "./Outputs",
                              train_prop = 0.75,
                              seed = 123) {
  
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
  
  # set seed and partition dataset into train and test splits
  set.seed(seed)
  train_size <- floor(train_prop * nrow(species_data))
  train_indices <- sample(seq_len(nrow(species_data)), size = train_size)
  
  train_data <- species_data[train_indices, ]
  test_data <- species_data[-train_indices, ]
  
  cat("Data partition complete: ", nrow(train_data), " train rows, ", nrow(test_data), " test rows.\n", sep = "")
  
  # 2. Select top models from previous results ----
  
  if (!file.exists(results_file)) {
    cat("ERROR: Previous results file 1 not found at ", results_file, "\n", sep = "")
    return(NULL)
  }
  if (!file.exists(results_file_2)) {
    cat("ERROR: Previous results file 2 not found at ", results_file_2, "\n", sep = "")
    return(NULL)
  }
  
  model_metrics_1 <- read.csv(results_file)
  # NOTA: En el futuro, results_file_2 deberá ser sustituido por el archivo del mejor modelo local extendido.
  model_metrics_2 <- read.csv(results_file_2)
  
  if (nrow(model_metrics_1) == 0 || nrow(model_metrics_2) == 0) {
    cat("ERROR: One of the results files is empty.\n")
    return(NULL)
  }
  
  # select the best model from each file
  best_model_1 <- model_metrics_1 %>%
    dplyr::arrange(AIC) %>%
    dplyr::slice(1) %>%
    dplyr::pull(Modelo)
  
  best_model_2 <- model_metrics_2 %>%
    dplyr::arrange(AIC) %>%
    dplyr::slice(1) %>%
    dplyr::pull(Modelo)
  
  cat("Models selected for fitting: \n")
  cat("  1. ", best_model_1, " (de results_file)\n", sep = "")
  cat("  2. ", best_model_2, " (de results_file_2)\n", sep = "")
  cat("NOTA: results_file_2 deberá ser sustituido en el futuro por el del modelo local extendido.\n")
  
  # retrieve equations from the compiled list
  all_equations <- get_models_list()
  
  # 3. Fit Model 1 on training split ----
  
  cat("Fitting Local Model 1 (", best_model_1, ") on training split...\n", sep = "")
  
  model_func_1 <- all_equations[[best_model_1]]$func
  start_params_1 <- all_equations[[best_model_1]]$start
  param_names_1 <- names(start_params_1)
  
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
  
  # 4. Fit Model 2 on training split ----
  
  # NOTA: Este modelo (Modelo Local 2) deberá ser sustituido en el futuro por el mejor modelo local extendido.
  cat("Fitting Local Model 2 (", best_model_2, ") on training split...\n", sep = "")
  cat("NOTA: Este modelo (Modelo Local 2) deberá ser sustituido en el futuro por el mejor modelo local extendido.\n")
  
  model_func_2 <- all_equations[[best_model_2]]$func
  start_params_2 <- all_equations[[best_model_2]]$start
  param_names_2 <- names(start_params_2)
  
  # build formula dynamically
  formula_str_2 <- paste0("h ~ model_func_2(", paste(param_names_2, collapse = ", "), ", dbh)")
  formula_2 <- as.formula(formula_str_2)
  
  fit_2 <- tryCatch({
    minpack.lm::nlsLM(formula_2, data = train_data, start = start_params_2)
  }, error = function(e) {
    attempt <- 1
    fit_success <- FALSE
    while (attempt <= 10 && !fit_success) {
      adjusted_start <- adjust_start(start_params_2)
      fit_try <- tryCatch({
        minpack.lm::nlsLM(formula_2, data = train_data, start = adjusted_start)
      }, error = function(err) NULL)
      if (!is.null(fit_try)) {
        fit_success <- TRUE
        return(fit_try)
      }
      attempt <- attempt + 1
    }
    return(NULL)
  })
  
  if (is.null(fit_2)) {
    cat("ERROR: Failed to fit Local Model 2 on training split.\n")
    return(NULL)
  }
  
  # 5. Fit Model 4 (Baseline local Power Model) on training split ----
  
  # NOTA: Este modelo (Modelo de Referencia / Base) deberá ser sustituido en el futuro por el modelo JCyL.
  cat("Fitting Baseline Model 4 (Potencia) on training split...\n")
  cat("NOTA: Este modelo (Modelo de Referencia / Base) deberá ser sustituido en el futuro por el modelo JCyL.\n")
  
  fit_4 <- tryCatch({
    minpack.lm::nlsLM(h ~ power_model_base(a, b, dbh), data = train_data, start = list(a = 1.3, b = 0.8))
  }, error = function(e) {
    cat("ERROR: Failed to fit Baseline Model 4.\n")
    return(NULL)
  })
  
  # 6. Predict on test dataset ----
  
  cat("Running predictions on test split...\n")
  
  # predict local model heights
  test_data$pred_h1 <- stats::predict(fit_1, newdata = test_data)
  test_data$pred_h2 <- stats::predict(fit_2, newdata = test_data)
  test_data$pred_h4 <- stats::predict(fit_4, newdata = test_data)
  
  # predict general model (Aitor 2025) using pre-calculated parameters
  test_data_general <- predict_height(
    df = test_data,
    path_to_pars = "./data/",
    dbh_col = "dbh",
    species_value = "code",
    species_col = "species"
  )
  test_data$pred_h3 <- test_data_general$pred_h
  
  # 7. Calculate evaluation metrics on test split ----
  
  metrics_1 <- calc_metrics(test_data$h, test_data$pred_h1, paste0("Local 1: ", best_model_1))
  metrics_2 <- calc_metrics(test_data$h, test_data$pred_h2, paste0("Local 2 (Sustituir por Local Ext.): ", best_model_2))
  metrics_3 <- calc_metrics(test_data$h, test_data$pred_h3, "General: Aitor 2025")
  metrics_4 <- calc_metrics(test_data$h, test_data$pred_h4, "Referencia (Sustituir por JCyL): Potencia")
  
  comparison_table <- dplyr::bind_rows(metrics_1, metrics_2, metrics_3, metrics_4)
  
  # ensure output folders exist and save results table
  comparativa_dir <- file.path(output_dir, "Graficos_Comparativa")
  dir.create(comparativa_dir, recursive = TRUE, showWarnings = FALSE)
  
  csv_output_path <- file.path(output_dir, "comparativa_modelos_split.csv")
  write.csv(comparison_table, csv_output_path, row.names = FALSE)
  
  cat("Metrics table saved successfully at: ", csv_output_path, "\n", sep = "")
  
  # 8. Generate diagnostic comparison plots in Spanish ----
  
  cat("Generating comparison plots...\n")
  
  # plot 1: superimpose curves on the test scatter plot
  dbh_seq <- seq(min(test_data$dbh, na.rm = TRUE), max(test_data$dbh, na.rm = TRUE), length.out = 1000)
  
  # predictions for sequence (local models)
  seq_pred_1 <- stats::predict(fit_1, newdata = data.frame(dbh = dbh_seq))
  seq_pred_2 <- stats::predict(fit_2, newdata = data.frame(dbh = dbh_seq))
  seq_pred_4 <- stats::predict(fit_4, newdata = data.frame(dbh = dbh_seq))
  
  # predictions for sequence (general model)
  seq_df <- data.frame(dbh = dbh_seq, species = species_id)
  seq_df_general <- predict_height(
    df = seq_df,
    path_to_pars = "./data/",
    dbh_col = "dbh",
    species_value = "code",
    species_col = "species"
  )
  seq_pred_3 <- seq_df_general$pred_h
  
  curves_data <- data.frame(
    dbh = rep(dbh_seq, 4),
    h_pred = c(seq_pred_1, seq_pred_2, seq_pred_3, seq_pred_4),
    Modelo = rep(c(
      paste0("Local 1: ", best_model_1),
      paste0("Local 2 (Sust. por Local Ext.): ", best_model_2),
      "General: Aitor 2025",
      "Referencia (Sust. por JCyL): Potencia"
    ), each = length(dbh_seq))
  )
  
  plot_curves <- ggplot2::ggplot() +
    ggplot2::geom_point(data = test_data, ggplot2::aes(x = dbh, y = h), color = "gray60", alpha = 0.5, size = 1.5) +
    ggplot2::geom_line(data = curves_data, ggplot2::aes(x = dbh, y = h_pred, color = Modelo), linewidth = 1.1) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Comparación de curvas h-d sobre datos de test",
      subtitle = paste0(species_name, " (25% datos de test)"),
      x = "Diámetro (cm)",
      y = "Altura (m)",
      color = "Modelo"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      legend.position = "bottom"
    )
  
  # plot 2: observed vs predicted heights panel plot (2x2)
  pred_long <- test_data %>%
    dplyr::select(dbh, h, pred_h1, pred_h2, pred_h3, pred_h4) %>%
    tidyr::pivot_longer(
      cols = c(pred_h1, pred_h2, pred_h3, pred_h4),
      names_to = "model_id",
      values_to = "h_pred"
    ) %>%
    dplyr::mutate(
      Modelo = dplyr::case_when(
        model_id == "pred_h1" ~ paste0("Local 1: ", best_model_1),
        model_id == "pred_h2" ~ paste0("Local 2 (Sust. por Local Ext.): ", best_model_2),
        model_id == "pred_h3" ~ "General: Aitor 2025",
        model_id == "pred_h4" ~ "Referencia (Sust. por JCyL): Potencia"
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
  
  # plot 3: violin and boxplot of residuals by model
  residuals_data <- pred_long %>%
    dplyr::mutate(residuals = h_pred - h)
  
  plot_violin_res <- ggplot2::ggplot(residuals_data, ggplot2::aes(x = Modelo, y = residuals, fill = Modelo)) +
    ggplot2::geom_violin(alpha = 0.6, trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.15, fill = "white", outlier.alpha = 0.3, alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1.1) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::labs(
      title = "Distribución de residuales por modelo (Violín)",
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
  
  # save plots
  ggplot2::ggsave(file.path(comparativa_dir, "comparativa_curvas_hd.png"), plot_curves, dpi = 300, width = 8, height = 6)
  ggplot2::ggsave(file.path(comparativa_dir, "comparativa_obs_vs_pred.png"), plot_obs_pred, dpi = 300, width = 10, height = 8)
  ggplot2::ggsave(file.path(comparativa_dir, "comparativa_violin_residuales.png"), plot_violin_res, dpi = 300, width = 8, height = 6)
  
  cat("All comparison plots saved successfully in: ", comparativa_dir, "\n", sep = "")
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    fit_1 = fit_1,
    fit_2 = fit_2,
    fit_4 = fit_4,
    metrics = comparison_table,
    plot_curves = plot_curves,
    plot_obs_pred = plot_obs_pred,
    plot_violin_res = plot_violin_res
  ))
}

# Example of standalone run ====

res <- compare_hd_models(
  data_path = "./data/1_harmonized_tree_df_jcyl_sfni.csv",
  results_file = "./data/21/Modelos95_resultados_modelos.csv",
  results_file_2 = "./data/21/Modelos95_resultados_modelos.csv",
  species_id = 21,
  species_name = "Pinus sylvestris",
  output_dir = "./Outputs"
)
