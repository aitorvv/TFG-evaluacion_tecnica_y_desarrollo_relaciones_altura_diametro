library(tidyverse)  
library(broom)  
library(ggplot2)    
library(openxlsx)
library(minpack.lm)
library(nlme)
library(broom.mixed)  

#read data
#(Pinus sylvestris, 21; Pinus nigra, 25; Pinus pinaster, 26; Quercus pyrenaica, 43)

sp <-26
sp_name <- 'Pinus pinaster' 
base_dir <- "/home/alicia/Uni/TFG/Modelo_extendido" #for the path in csv exportation

tree_data <- read.csv("/home/alicia/Uni/TFG/Modelo_extendido/Data/tree_data.csv")
tree_data <- tree_data[tree_data$species == sp, ]
tree_data <- tree_data[tree_data$INVENTORY_ID != 'JCyL_SEN', ]
stats_csv <- file.path(base_dir, "output", sp, paste0(sp, "_nlme_models_stats.csv"))
source('/home/alicia/Uni/TFG/Modelo_extendido/Data/2.0_hd_equations.r')
hd_models_collection <- get_models_list()

### gráficos ajuste y residuales

# Función para seleccionar el mejor modelo extendido y generar diagnósticos ====

#' Selecciona el mejor modelo extendido (nlme/gnls) por AIC y genera gráficos de diagnóstico
#'
#' @param tree_data Dataframe con datos de árboles y variables de masa ya unidos.
#' @param species_id Numeric o character con el ID de especie (e.g., 21).
#' @param species_name Character con el nombre de la especie (e.g., "Pinus sylvestris").
#' @param results_csv Ruta al CSV con las métricas de todos los modelos ajustados.
#' @param models_dir Directorio donde se guardaron los RDS de los modelos ajustados.
#' @param output_dir Directorio raíz donde guardar los gráficos.
#' @param hd_models_collection Lista de modelos h-d obtenida con get_models_list().
#' @param exclude_vars Vector de covariables a excluir (p.ej. "Ho"). NULL = sin filtro.
#' @return Lista con el nombre del modelo, el objeto de ajuste, las métricas y los gráficos.
plot_best_extended_hd_model <- function(tree_data,
                                        species_id,
                                        species_name,
                                        results_csv,
                                        models_dir,
                                        output_dir,
                                        hd_models_collection,
                                        exclude_vars = NULL) {
  
  cat("Seleccionando el mejor modelo extendido para", species_name, "(ID:", species_id, ")...\n")
  
  # 1. Leer resultados y seleccionar el mejor modelo por AIC ====
  
  if (!file.exists(results_csv)) {
    cat("ERROR: No se encuentra el CSV de resultados en", results_csv, "\n")
    return(NULL)
  }
  
  model_metrics <- read.csv(results_csv)
  model_metrics <- model_metrics[model_metrics$species == species_id & !model_metrics$error, ]
  
  # excluir modelos que usen ciertas covariables (p.ej. "Ho")
  if (!is.null(exclude_vars)) {
    for (v in exclude_vars) {
      model_metrics <- model_metrics[!grepl(paste0("\\b", v, "\\b"), model_metrics$fe_combi), ]
    }
  }
  # excluir combinaciones
  forbidden_pairs <- list(
    c("N",  "SDI"),
    c("Do", "dg")
  )
  
  if (!is.null(forbidden_pairs) && nrow(model_metrics) > 0) {
    has_var <- function(combi, v) grepl(paste0("\\b", v, "\\b"), combi)
    keep <- rep(TRUE, nrow(model_metrics))
    for (pair in forbidden_pairs) {
      both <- has_var(model_metrics$fe_combi, pair[1]) &
        has_var(model_metrics$fe_combi, pair[2])
      keep <- keep & !both
    }
    n_drop <- sum(!keep)
    if (n_drop > 0)
      cat("Filtro de pares incompatibles: descartados", n_drop, "modelos.\n")
    model_metrics <- model_metrics[keep, ]
  }
  if (nrow(model_metrics) == 0) {
    cat("ERROR: No quedan modelos válidos para la especie", species_id,
        if (!is.null(exclude_vars)) paste0(" tras excluir ", paste(exclude_vars, collapse = ", ")) else "",
        "\n")
    return(NULL)
  }
  
  best_row <- model_metrics[which.min(model_metrics$aic), ]
  best_model_name <- best_row$model_name
  best_fe_i      <- best_row$n_fe_combi
  best_re_j      <- best_row$n_re_combi
  
  cat("Mejor modelo:", best_model_name,
      "| fe:", best_fe_i, "| re:", best_re_j,
      "| AIC:", round(best_row$aic, 2), "\n")
  
 
  # 2. Cargar el modelo: disco -> si falla la lectura, objeto del Environment ====
  
  # nombre del fichero en disco (species_id PEGADO, como se guardaron los RDS)
  rds_path <- file.path(models_dir,
                        paste0("output/", species_id, "/nlme_hd_models", species_id,
                               "_", best_model_name,
                               "_fe_", best_fe_i,
                               "_re_", best_re_j, ".rds"))
  
  # nombre del objeto en el Environment (SIN species_id, con guion bajo)
  obj_name <- paste0("nlme_hd_models_", best_model_name,
                     "_fe_", best_fe_i, "_re_", best_re_j)
  
  best_fit <- NULL
  
  # 2a. intentar leer del disco; si la conexión falla, NO rompe (tryCatch)
  if (file.exists(rds_path)) {
    best_fit <- tryCatch(
      readRDS(rds_path),
      error = function(e) {
        cat("Aviso: el RDS existe pero no se pudo leer (", conditionMessage(e), ")\n", sep = "")
        NULL
      }
    )
  }
  
  # 2b. si no hay modelo desde disco, buscar el objeto cargado en el Environment
  if (is.null(best_fit)) {
    if (exists(obj_name, envir = .GlobalEnv, inherits = FALSE)) {
      cat("Usando objeto del Environment:", obj_name, "\n")
      best_fit <- get(obj_name, envir = .GlobalEnv)
    } else {
      cat("ERROR: no hay RDS legible ni objeto '", obj_name, "' en el Environment.\n", sep = "")
      return(NULL)
    }
  }
  
  # 2c. reconstruir el entorno para que predict() funcione con el objeto cargado ====
  env_fit <- environment(formula(best_fit))
  
  model_function <- hd_models_collection[[best_model_name]]$func
  assign("model_function", model_function, envir = env_fit)
  
  # ajustes para asOneFormula
  fe_combi <- lapply(strsplit(best_row$fe_combi, "; ")[[1]], as.formula)
  best_fit$call$fixed <- fe_combi
  if (!is.null(best_row$re_combi) && best_row$re_combi != "none") {
    best_fit$call$random <- as.formula(best_row$re_combi)
  }
  
  # CLAVE: que 'fe_combi' (y model_function) vivan en el entorno donde predict() los busca
  assign("fe_combi", fe_combi, envir = env_fit)
  
  # 3. Filtrar datos de la especie ====
  species_data <- tree_data[tree_data$species == species_id, ]
  
  if (nrow(species_data) == 0) {
    cat("ERROR: No hay datos para la especie", species_id, "\n")
    return(NULL)
  }
  
  # El ajuste se hizo sobre complete.cases de TODAS las covariables
  # (mismo N para todos los modelos) -> replicamos ese filtro para alinear con predict()
  covs <- c("G", "N", "SDI", "dg", "Do", "Ho", "ALT", "dbh", "h")
  covs <- intersect(covs, names(species_data))   # por si algún nombre no existe
  
  na_mask <- !complete.cases(species_data[, covs, drop = FALSE])
  
  if (any(na_mask)) {
    cat("Filas con NA en covariables:", sum(na_mask), "\n")
    cat("NA por columna:\n"); print(colSums(is.na(species_data[, covs, drop = FALSE])))
    if ("PLOT_ID" %in% names(species_data))
      cat("PLOT_ID afectados:", paste(unique(species_data$PLOT_ID[na_mask]), collapse = ", "), "\n")
    species_data <- species_data[!na_mask, , drop = FALSE]
    cat("Tras filtrar NA:", nrow(species_data), "filas.\n")
  }
  
  # calcular predicciones y residuales
  species_data$h_pred    <- predict(best_fit)
  species_data$residuals <- residuals(best_fit)
  species_data$h_pred_l0 <- predict(best_fit, level = 0)
  species_data$resid_l0  <- species_data$h - species_data$h_pred_l0
  
  # 4. Exportar métricas e información del mejor modelo ====
  
  best_metrics <- data.frame(
    species     = species_id,
    sp_name     = species_name,
    model_name  = best_model_name,
    fe_combi    = best_row$fe_combi,
    re_combi    = best_row$re_combi,
    start       = best_row$start,
    coefs       = paste(names(nlme::fixef(best_fit)),
                        round(nlme::fixef(best_fit), 5),
                        sep = "=", collapse = "; "),
    r_squared   = best_row$r_squared,
    rmse        = best_row$rmse,
    mae         = best_row$mae,
    bias        = best_row$bias,
    aic         = best_row$aic,
    bic         = best_row$bic,
    loglik      = best_row$loglik
  )
  
  suffix <- if (!is.null(exclude_vars)) paste0("_sin_", paste(exclude_vars, collapse = "_")) else ""
  plot_dir <- file.path(output_dir, species_id, paste0("Graficos_Seleccion_Extendido", suffix))
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  write.csv(best_metrics,
            file.path(plot_dir, "mejor_modelo_metricas.csv"),
            row.names = FALSE)
  
  cat("Métricas del mejor modelo exportadas.\n")
  
  # 5. Subtítulo común para todos los gráficos ====
  
  subtitle_text <- paste0(species_name, " - Modelo: ", best_model_name,
                          "\nEF fijos: ", best_row$fe_combi,
                          " | EF aleatorios: ", best_row$re_combi)
  
  # 6. Gráficos de diagnóstico ====
  
  # 6.1 Ajuste h-d (observados + predichos sobre datos reales) ----
  
  plot_fit <- ggplot2::ggplot(species_data, ggplot2::aes(x = dbh, y = h)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(y = h_pred), color = "red", size = 1.2, alpha = 0.4, shape = 4) +
    ggplot2::xlim(0, max(species_data$dbh, na.rm = TRUE)) +
    ggplot2::ylim(0, max(species_data$h, na.rm = TRUE)) +
    ggplot2::labs(
      title    = "Ajuste del modelo h-d extendido",
      subtitle = subtitle_text,
      x = "Diámetro (cm)", y = "Altura (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.1.1 Ajuste h-d - curva única (efectos fijos, level = 0) ----
  
  dbh_seq <- seq(min(species_data$dbh, na.rm = TRUE),
                 max(species_data$dbh, na.rm = TRUE),
                 length.out = 1000)
  
  # plantilla = una fila real -> garantiza que TODAS las columnas existen y son válidas
  newdata_curve <- species_data[rep(1, length(dbh_seq)), , drop = FALSE]
  newdata_curve$dbh <- dbh_seq
  
  # covariables numéricas fijadas a su media (la "parcela media")
  num_cols <- names(species_data)[sapply(species_data, is.numeric)]
  for (cc in setdiff(num_cols, "dbh")) {
    newdata_curve[[cc]] <- mean(species_data[[cc]], na.rm = TRUE)
  }
  
  # level = 0: solo efectos fijos (con esto el nivel de INVENTORY_ID no afecta)
  pred_h_fixed <- predict(best_fit, newdata = newdata_curve, level = 0)
  df_curve <- data.frame(dbh_seq = dbh_seq, pred_h = pred_h_fixed)
  
  plot_fit_curve <- ggplot2::ggplot(species_data, ggplot2::aes(x = dbh, y = h)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_line(data = df_curve, ggplot2::aes(x = dbh_seq, y = pred_h),
                       color = "red", linewidth = 1.1) +
    ggplot2::xlim(0, max(species_data$dbh, na.rm = TRUE)) +
    ggplot2::ylim(0, max(species_data$h, na.rm = TRUE)) +
    ggplot2::labs(
      title    = "Ajuste del modelo h-d extendido (curva media)",
      subtitle = subtitle_text,
      x = "Diámetro (cm)", y = "Altura (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.2 Observados vs. predichos ----
  
  max_h <- max(c(species_data$h, species_data$h_pred), na.rm = TRUE)
  
  plot_obs_pred <- ggplot2::ggplot(species_data, ggplot2::aes(x = h_pred, y = h)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1.1) +
    ggplot2::xlim(0, max_h) +
    ggplot2::ylim(0, max_h) +
    ggplot2::labs(
      title    = "Valores observados vs. predichos",
      subtitle = subtitle_text,
      x = "Altura predicha (m)", y = "Altura observada (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.3 Residuales vs. predicciones ----
  
  plot_res_pred <- ggplot2::ggplot(species_data, ggplot2::aes(x = h_pred, y = residuals)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 1.1, linetype = "dashed") +
    ggplot2::labs(
      title    = "Residuales vs. predicciones",
      subtitle = subtitle_text,
      x = "Altura predicha (m)", y = "Residuales (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.4 Residuales vs. diámetro ----
  
  plot_res_dbh <- ggplot2::ggplot(species_data, ggplot2::aes(x = dbh, y = residuals)) +
    ggplot2::geom_point(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 1.1, linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, color = "darkblue", se = TRUE, linewidth = 0.9) +
    ggplot2::labs(
      title    = "Residuales vs. diámetro",
      subtitle = subtitle_text,
      x = "Diámetro (cm)", y = "Residuales (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.5 Distribución de residuales ----
  
  plot_res_dist <- ggplot2::ggplot(species_data, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30, fill = "forestgreen", alpha = 0.6, color = "white") +
    ggplot2::geom_density(color = "red", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    ggplot2::labs(
      title    = "Distribución de residuales",
      subtitle = subtitle_text,
      x = "Residuales (m)", y = "Densidad"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 6.6 Q-Q de residuales ----
  
  plot_qq <- ggplot2::ggplot(species_data, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq(color = "forestgreen", size = 2, alpha = 0.5) +
    ggplot2::stat_qq_line(color = "red", linewidth = 1.1) +
    ggplot2::labs(
      title    = "Gráfico Q-Q de residuales",
      subtitle = subtitle_text,
      x = "Cuantiles teóricos", y = "Cuantiles muestrales"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
      axis.text.x   = ggplot2::element_text(size = 13),
      axis.text.y   = ggplot2::element_text(size = 13)
    )
  
  # 7. Guardar gráficos individuales ====
  
  ggplot2::ggsave(file.path(plot_dir, "1_ajuste.png"),                     plot_fit,       dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "2_observados_vs_predichos.png"),    plot_obs_pred,  dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "3_residuales_vs_predicciones.png"), plot_res_pred,  dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "4_residuales_vs_diametro.png"),     plot_res_dbh,   dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "5_distribucion_residuales.png"),    plot_res_dist,  dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "6_qq_residuales.png"),              plot_qq,        dpi = 300, width = 7, height = 5)
  ggplot2::ggsave(file.path(plot_dir, "1b_ajuste_curva_media.png"),        plot_fit_curve, dpi = 300, width = 7, height = 5)
  
  # 8. Guardar bloques ====
  
  block_1 <- ggpubr::ggarrange(plot_fit, plot_res_pred, ncol = 2, nrow = 1, labels = c("A", "B"))
  ggplot2::ggsave(file.path(plot_dir, "bloque_1_ajuste_prediccion.png"),
                  block_1, dpi = 300, width = 14, height = 6)
  
  block_1b <- ggpubr::ggarrange(plot_fit_curve, plot_res_pred, ncol = 2, nrow = 1, labels = c("A", "B"))
  ggplot2::ggsave(file.path(plot_dir, "bloque_1b_ajuste_curva_media.png"),
                  block_1b, dpi = 300, width = 14, height = 6)
  
  block_2 <- ggpubr::ggarrange(plot_obs_pred, plot_res_dbh, plot_res_dist, plot_qq,
                               ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
  ggplot2::ggsave(file.path(plot_dir, "bloque_2_diagnostico_residuales.png"),
                  block_2, dpi = 300, width = 14, height = 12)
  
  cat("Gráficos guardados en", plot_dir, "\n")
  
  # 9. Retornar resultados ====
  
  return(list(
    model_name     = best_model_name,
    model_fit      = best_fit,
    best_metrics   = best_metrics,
    plot_fit       = plot_fit,
    plot_fit_curve = plot_fit_curve,
    plot_obs_pred  = plot_obs_pred,
    plot_res_pred  = plot_res_pred,
    plot_res_dbh   = plot_res_dbh,
    plot_res_dist  = plot_res_dist,
    plot_qq        = plot_qq,
    block_1        = block_1,
    block_1b       = block_1b,
    block_2        = block_2,
    pred_data      = species_data
  ))
}


# Ejemplo de ejecución ====

# 1º modelo: el mejor global
best_extended <- plot_best_extended_hd_model(
  tree_data            = tree_data,
  species_id           = sp,
  species_name         = sp_name,
  results_csv          = stats_csv,
  models_dir           = base_dir,
  output_dir           = file.path(base_dir, "output"),
  hd_models_collection = hd_models_collection
)

# 2º modelo: el mejor SIN Ho
best_extended_sin_ho <- plot_best_extended_hd_model(
  tree_data            = tree_data,
  species_id           = sp,
  species_name         = sp_name,
  results_csv          = stats_csv,
  models_dir           = base_dir,
  output_dir           = file.path(base_dir, "output"),
  hd_models_collection = hd_models_collection,
  exclude_vars         = "Ho"
)

# Exportar parámetros y estadísticos de ambos en un único CSV
resumen <- rbind(best_extended$best_metrics,
                 best_extended_sin_ho$best_metrics)

write.csv(resumen,
          file.path(base_dir, "output", sp, paste0(sp, "_mejores_modelos.csv")),
          row.names = FALSE)

################################################################################

library(dplyr)

comparacion <- best_extended$pred_data %>%   # TODO species_data, sin tocar nombres
  left_join(
    best_extended_sin_ho$pred_data %>%
      select(TREE_ID, h_pred, residuals, h_pred_l0, resid_l0) %>%
      rename(h_pred_sin_ho   = h_pred,
             resid_sin_ho    = residuals,
             h_pred_l0_sin_ho = h_pred_l0,
             resid_l0_sin_ho  = resid_l0),
    by = "TREE_ID"
  )

write.csv(comparacion,
          file.path(base_dir, "output", sp, paste0(sp, "_predicciones_comparacion.csv")),
          row.names = FALSE)
