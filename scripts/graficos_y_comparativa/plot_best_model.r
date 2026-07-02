#!/usr/bin/Rscript

# Code to select the best height-diameter model and plot results ----
#
# Aitor Vázquez Veloso
# 2026-06-17
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Load libraries ====

library(tidyverse)
library(ggpubr)
source("./2.0_hd_equations.r")


# Define functions ====

#' Select the best model by AIC and generate all diagnostic plots in Spanish
#'
#' @param trees_data Dataframe containing tree measurements (must include dbh and h).
#' @param species_id Numeric or character ID of the species (e.g., 21).
#' @param species_name Character string of the species name (e.g., "Pinus sylvestris").
#' @param results_dir Character string of the output directory where results are saved.
#' @return A list containing the best model name, the loaded model object, and the plots.
#' @examples
#' # results <- plot_best_hd_model(trees, 21, "Pinus sylvestris", "./data")
#' @export
plot_best_hd_model <- function(trees_data, species_id, species_name, results_dir = "./data") {
  
  # track progress
  cat("Selecting the best model for species ", species_name, " (ID: ", species_id, ")...\n", sep = "")
  
  # check directory and results file
  species_dir <- file.path(results_dir, species_id)
  results_file <- file.path(species_dir, "Modelos95_resultados_modelos.csv")
  
  if (!file.exists(results_file)) {
    cat("ERROR: Results CSV file not found at ", results_file, "\n", sep = "")
    return(NULL)
  }
  
  # read the model evaluation results
  model_metrics <- read.csv(results_file)
  
  if (nrow(model_metrics) == 0) {
    cat("ERROR: Results CSV is empty.\n")
    return(NULL)
  }
  
  # sort by AIC and select the best model
  best_model_row <- model_metrics %>%
    dplyr::arrange(AIC) %>%
    dplyr::slice(1)
  
  best_model_name <- best_model_row$Modelo
  best_model_aic <- best_model_row$AIC
  
  cat("Best model selected: ", best_model_name, " (AIC: ", round(best_model_aic, 2), ")\n", sep = "")
  
  # load the best model RDS
  best_model_path <- file.path(species_dir, "Ajuste", paste0("fit_", best_model_name, ".RData"))
  
  if (!file.exists(best_model_path)) {
    cat("ERROR: Model fit file not found at ", best_model_path, "\n", sep = "")
    return(NULL)
  }
  
  # load model equations to bind the model_function environment for predict()
  models_list <- get_models_list()
  model_function <- models_list[[best_model_name]]$func
  start_params_orig <- models_list[[best_model_name]]$start
  param_names <- names(start_params_orig)
  formula_str <- paste0("h ~ model_function(", paste(param_names, collapse = ", "), ", dbh)")
  model_formula <- as.formula(formula_str) #ecuación a usar
  
  # load model object (note: fit was saved with saveRDS, so we read it with readRDS)
  best_fit <- readRDS(best_model_path)
  assign("model_function", model_function, envir = environment(formula(best_fit)))
  
  # filtered tree data for the specific species and active trees
  species_trees <- trees_data %>%
    dplyr::filter(species == species_id & dead == 0)
  
  if (nrow(species_trees) == 0) {
    cat("ERROR: No tree data found for species ", species_id, "\n", sep = "")
    return(NULL)
  }
  
  # calculate model predictions and residuals for diagnostics
  eval_data <- species_trees
  eval_data$h_pred <- stats::predict(best_fit)
  eval_data$residuals <- stats::residuals(best_fit)
  
  # 1. Height-diameter fit plot ----
  
  # generate a sequence of diameters to plot a smooth curve
  dbh_seq <- seq(min(species_trees$dbh, na.rm = TRUE), max(species_trees$dbh, na.rm = TRUE), length.out = 1000)
  pred_h <- stats::predict(best_fit, newdata = data.frame(dbh = dbh_seq))
  df_artificial <- data.frame(dbh_seq = dbh_seq, pred_h = pred_h)
  
  plot_fit <- ggplot2::ggplot(species_trees, ggplot2::aes(x = dbh, y = h)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_line(data = df_artificial, ggplot2::aes(x = dbh_seq, y = pred_h), color = "red", linewidth = 1.1) +
    ggplot2::xlim(0, max(species_trees$dbh, na.rm = TRUE)) +
    ggplot2::ylim(0, max(species_trees$h, na.rm = TRUE)) +
    ggplot2::labs(
      title = "Ajuste del modelo h-d",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Diámetro (cm)",
      y = "Altura (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # 2. Observed vs Predicted Height ----
  
  plot_obs_pred <- ggplot2::ggplot(eval_data, ggplot2::aes(x = h_pred, y = h)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1.1) +
    ggplot2::labs(
      title = "Valores observados vs. predichos",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Altura predicha (m)",
      y = "Altura observada (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # 3. Residuals vs Predictions ----
  
  plot_res_pred <- ggplot2::ggplot(eval_data, ggplot2::aes(x = h_pred, y = residuals)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 1.1, linetype = "dashed") +
    ggplot2::labs(
      title = "Residuales vs. predicciones",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Altura predicha (m)",
      y = "Residuales (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # 4. Residuals vs Diameter ----
  
  plot_res_dbh <- ggplot2::ggplot(eval_data, ggplot2::aes(x = dbh, y = residuals)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 1.1, linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, color = "darkblue", se = TRUE, linewidth = 0.9) +
    ggplot2::labs(
      title = "Residuales vs. diámetro",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Diámetro (cm)",
      y = "Residuales (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # 5. Residual Distribution (Histogram and Density) ----
  
  plot_res_dist <- ggplot2::ggplot(eval_data, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = "forestgreen", alpha = 0.6, color = "white") +
    ggplot2::geom_density(color = "red", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    ggplot2::labs(
      title = "Distribución de residuales",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Residuales (m)",
      y = "Densidad"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # 6. Quantile-Quantile (Q-Q) Plot of Residuals ----
  
  plot_qq <- ggplot2::ggplot(eval_data, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::stat_qq_line(color = "red", linewidth = 1.1) +
    ggplot2::labs(
      title = "Gráfico Q-Q de residuales",
      subtitle = paste0(species_name, " - Modelo: ", best_model_name),
      x = "Cuantiles teóricos",
      y = "Cuantiles muestrales"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x = ggplot2::element_text(size = 13),
      axis.text.y = ggplot2::element_text(size = 13)
    )
  
  # Ensure output directories exist ====
  
  plot_dir <- file.path(species_dir, "Graficos_Seleccion")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save individual plots ====
  
  ggplot2::ggsave(file.path(plot_dir, "1_ajuste.png"), plot_fit, dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "2_observados_vs_predichos.png"), plot_obs_pred, dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "3_residuales_vs_predicciones.png"), plot_res_pred, dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "4_residuales_vs_diametro.png"), plot_res_dbh, dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "5_distribucion_residuales.png"), plot_res_dist, dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "6_qq_residuales.png"), plot_qq, dpi = 300, width = 7, height = 5)
  
  # Save Block 1: Fit and Observed vs Predicted (1 row, 2 columns) ====
  
  block_1 <- ggpubr::ggarrange(
    plot_fit,
    plot_obs_pred,
    ncol = 2,
    nrow = 1,
    labels = c("A", "B")
  )
  
  block_1_path <- file.path(plot_dir, "bloque_1_ajuste_prediccion.png")
  ggplot2::ggsave(
    filename = block_1_path,
    plot = block_1,
    dpi = 300,
    width = 14,
    height = 6
  )
  
  # Save Block 2: The remaining 4 diagnostic plots (2 rows, 2 columns) ====
  
  block_2 <- ggpubr::ggarrange(
    plot_res_pred,
    plot_res_dbh,
    plot_res_dist,
    plot_qq,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D")
  )
  
  block_2_path <- file.path(plot_dir, "bloque_2_diagnostico_residuales.png")
  ggplot2::ggsave(
    filename = block_2_path,
    plot = block_2,
    dpi = 300,
    width = 14,
    height = 12
  )
  
  cat("All individual plots and blocks saved successfully for species ", species_name, "\n", sep = "")
  
  # return list with model information
  return(list(
    model_name = best_model_name,
    model_fit = best_fit,
    plot_fit = plot_fit,
    plot_obs_pred = plot_obs_pred,
    plot_res_pred = plot_res_pred,
    plot_res_dbh = plot_res_dbh,
    plot_res_dist = plot_res_dist,
    plot_qq = plot_qq,
    block_1 = block_1,
    block_2 = block_2
  ))
}

# Example of script execution ====

# Load data
trees <- read.csv("./data/1_harmonized_tree_df_jcyl_sfni.csv")

# Select parameters
sp <- 21
sp_name <- "Pinus sylvestris"

# Run selection and generate all plots
best_model_results <- plot_best_hd_model(
  trees_data = trees,
  species_id = sp,
  species_name = sp_name,
  results_dir = "./Outputs"
)
