#!/usr/bin/Rscript


# ----- Impacto del modelo h-d en volumen, biomasa, carbono y € (€/ha)-----

#
#  Parte del CSV de alturas predichas que genera la fase anterior
#  (<sp_id>_alturas_predichas_test.csv).
#
#  Lógica (sobre la MUESTRA DE VALIDACIÓN):
#    1. Calcula vol / biomasa / carbono con la ALTURA OBSERVADA (= referencia).
#    2. Repite el cálculo con las 5 alturas predichas (pred_h1..pred_h5).
#    3. El error de cada modelo es la desviación FRENTE A la magnitud calculada
#       con la altura observada -> aísla la contribución del modelo h-d
#       (mismo dbh, misma ecuación de cubicación, solo cambia h).
#
#  Outputs:
#    - VCC (pinos) / VLE (Quercus) en m³ (ecuaciones IFN4, por provincia).
#    - Biomasa aérea TOTAL W_T en kg (Ruiz-Peinado 2011 coníferas / 2012 frondosas).
#    - Carbono = W_T * fracción C (Montero 2005); CO2 = C * 44/12.
#    - €: madera de sierra 50 €/m³ (pinos); leñas 50 €/t -> €/m³ con dens. 0.64 t/m³
#         (Quercus); pellet 5.25 €/15 kg -> €/t; CO2 73.95 €/t.
#    - Resultado final en €/ha a escala de parcela (requiere factor de expansión).


library(tidyverse)

# Funciones de cubicación / biomasa (silviculture)
library(silviculture)
source('/home/alicia/Uni/TFG/Compare/functions/sfni_volume/snfi_support_functions.r')
source('/home/alicia/Uni/TFG/Compare/functions/sfni_volume/snfi_volume_equations.r')

# ============================ AJUSTES =======================================
sp_id   <- 43
sp_name <- "Quercus pyrenaica"   # (21 Pinus sylvestris, 25 P.nigra, 26 P.pinaster, 43 Quercus pyrenaica)

pred_csv    <- paste0("/home/alicia/Uni/TFG/Compare/Outputs/Compare_models/", sp_id, "_alturas_predichas_test.csv")
metrics_csv <- paste0("/home/alicia/Uni/TFG/Compare/Outputs/Compare_models/", sp_id, "_comparativa_modelos_split.csv") # para etiquetar modelos
out_dir     <- file.path("/home/alicia/Uni/TFG/Compare/Outputs", "Balance_VBC")

# --- Producto principal de volumen ---
# pinos  -> vcc (madera con corteza) ;  Quercus -> vle (leñas)
vol_col <- if (sp_id == 43) "vle" else "vcc"

# --- Calidad/forma IFN4 (anexo 15): coníferas = 2 ; Q. pyrenaica = 4 ---
quality_code <- if (sp_id == 43) 4 else 2

# --- Biomasa aérea TOTAL (W_T), NO solo fuste ---
# OJO: confirma el string que acepta tu versión de silviculture (p.ej. "total").
# La fase de Claude usaba component = 'stem'; M&M pide W_T total.
biomass_component <- "tree"

# --- Precios (M&M, Tabla precios_madera) ---
precio_madera    <- 50     # pinos: €/m³ (sierra) | Quercus: €/t (leñas)
densidad_quercus <- 0.64   # t/m³, homogeneiza leñas a €/m³ (solo Quercus)
# €/m³ efectivo para el valor de madera:
factor_madera <- if (sp_id == 43) densidad_quercus * precio_madera else precio_madera

precio_pellet_t <- (5.25 / 15) * 1000   # 5.25 €/saco 15 kg -> 350 €/t
# (dens. pellet 650 kg/m³ -> 227.5 €/m³; solo si quieres el precio en €/m³)

precio_CO2   <- 73.95      # €/t CO2 (SENDECO2)
factor_C_CO2 <- 44 / 12    # C -> CO2

# Fracción de carbono sobre biomasa seca (Montero 2005), en %
carbon_pct <- dplyr::case_when(
  sp_name == "Pinus pinaster"                       ~ 51.1,
  sp_name %in% c("Pinus sylvestris", "Pinus nigra") ~ 50.9,
  sp_name == "Quercus pyrenaica"                    ~ 47.5,
  TRUE                                              ~ NA_real_
)


expan_candidates <- c("expan")
# ============================================================================


# --- 0. Cargar alturas predichas -------------------------------------------
if (!file.exists(pred_csv)) stop("No encuentro el CSV de alturas: ", pred_csv)
pred <- read.csv(pred_csv, stringsAsFactors = FALSE)

# columnas mínimas
stopifnot(all(c("PLOT_ID", "dbh", "h_obs",
                "pred_h1", "pred_h2", "pred_h3", "pred_h4", "pred_h5") %in% names(pred)))

# especie (el CSV es monoespecífico) y calidad fija por especie
pred$species <- sp_id
pred$quality <- quality_code

# provincia: Palencia = 34, León = 24 (M&M)
pred$province <- ifelse(grepl("_34_", pred$PLOT_ID) | grepl("JCyL", pred$PLOT_ID), 34, 24)
pred$province <- ifelse(unique(pred$species) == 26, 34, pred$province)

# factor de expansión -> €/ha
expan_col <- intersect(expan_candidates, names(pred))
if (length(expan_col) == 0) {
  warning("\n>>> No hay factor de expansión en el CSV. Los € NO son €/ha reales:\n",
          ">>> se agregan sobre los pies muestreados. Añade 'expan' al export de la\n",
          ">>> fase 1 (una línea) para tener €/ha correctos.\n")
  pred$expan <- 1
  has_expan <- FALSE
} else {
  names(pred)[names(pred) == expan_col[1]] <- "expan"
  has_expan <- TRUE
}

# etiquetas de modelo en el orden pred_h1..pred_h5
h_cols <- c("h_obs", "pred_h1", "pred_h2", "pred_h3", "pred_h4", "pred_h5")
if (file.exists(metrics_csv)) {
  modelo_labels <- c("Observada", read.csv(metrics_csv)$Modelo)
} else {
  modelo_labels <- c("Observada", paste0("Modelo ", 1:5))
}
names(modelo_labels) <- h_cols


# --- 1. Cálculo vol / biomasa / carbono para UNA altura --------------------
calc_vbc <- function(df, h_col) {
  
  vol <- silviculture::silv_predict_snfi_volume(
    province = df$province,
    species  = df$species,
    dbh      = df$dbh,
    h        = df[[h_col]],
    quality  = df$quality
  )
  vol_vec <- vol[[vol_col]]
  
  # biomasa aérea TOTAL (kg)
  if (sp_name %in% c("Pinus pinaster", "Pinus nigra", "Pinus sylvestris")) {
    wt <- silv_predict_biomass(
      diameter = df$dbh, height = df[[h_col]],
      model = eq_biomass_ruiz_peinado_2011(sp_name, component = biomass_component))
  } else {
    wt <- silv_predict_biomass(
      diameter = df$dbh, height = df[[h_col]],
      model = eq_biomass_ruiz_peinado_2012(sp_name, component = biomass_component))
  }
  
  carbon <- wt * (carbon_pct / 100)   # kg C
  data.frame(vol = vol_vec, biomasa = wt, carbono = carbon)
}

# 6 alturas -> 18 columnas físicas (vol/biomasa/carbono * 6)
vbc <- purrr::map(h_cols, function(hc) {
  out <- calc_vbc(pred, hc)
  names(out) <- paste0(c("vol", "biomasa", "carbono"), "_", hc)
  out
}) %>% dplyr::bind_cols()

vbc$PLOT_ID      <- pred$PLOT_ID
vbc$INVENTORY_ID <- if ("INVENTORY_ID" %in% names(pred)) pred$INVENTORY_ID else NA
vbc$expan        <- pred$expan


# --- 2. Valoración económica por pie (€), para cada altura -----------------
# Lineal: € = magnitud_física * precio. (El % de error en € == % en la magnitud.)
for (hc in h_cols) {
  vbc[[paste0("eur_madera_", hc)]] <-  (vbc[[paste0("vol_", hc)]] / 1000)     * factor_madera
  vbc[[paste0("eur_pellet_", hc)]] <- (vbc[[paste0("biomasa_", hc)]] / 1000) * precio_pellet_t
  vbc[[paste0("eur_co2_", hc)]]    <- (vbc[[paste0("carbono_", hc)]] / 1000) * factor_C_CO2 * precio_CO2
}


# ============================================================================
#  BLOQUE FÍSICO — error en vol / biomasa / carbono
#  Tres niveles de agregación: (a) global  (b) por parcela  (c) por pie
# ============================================================================
magnitudes <- c("vol", "biomasa", "carbono")
mag_lab    <- c(vol = "Volumen (m³)", biomasa = "Biomasa (kg)", carbono = "Carbono (kg)")
pred_h     <- c("pred_h1", "pred_h2", "pred_h3", "pred_h4", "pred_h5")

# Formato largo: una fila por (pie, modelo, magnitud) con obs y pred ponderados
error_long <- purrr::map_dfr(pred_h, function(hc) {
  purrr::map_dfr(magnitudes, function(mg) {
    obs  <- vbc[[paste0(mg, "_h_obs")]]
    prd  <- vbc[[paste0(mg, "_", hc)]]
    data.frame(
      PLOT_ID      = vbc$PLOT_ID,
      INVENTORY_ID = vbc$INVENTORY_ID,
      expan        = vbc$expan,
      Modelo       = modelo_labels[[hc]],
      Magnitud     = mg,
      obs          = obs,
      pred         = prd,
      obs_ha       = obs * vbc$expan,
      pred_ha      = prd * vbc$expan,
      error_pct    = (prd - obs) / obs * 100
    )
  })
})
error_long$Magnitud <- factor(error_long$Magnitud, levels = magnitudes, labels = mag_lab)

# (a) GLOBAL: sesgo agregado (lo que más pesa económicamente: errores se compensan)
tabla_global <- error_long %>%
  dplyr::group_by(Magnitud, Modelo) %>%
  dplyr::summarise(
    total_obs_ha  = sum(obs_ha,  na.rm = TRUE),
    total_pred_ha = sum(pred_ha, na.rm = TRUE),
    sesgo_pct     = (sum(pred_ha, na.rm = TRUE) - sum(obs_ha, na.rm = TRUE)) /
      sum(obs_ha, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# (b) POR PARCELA: error % de cada parcela, y su distribución
error_parcela <- error_long %>%
  dplyr::group_by(Magnitud, Modelo, PLOT_ID, INVENTORY_ID) %>%
  dplyr::summarise(
    obs_plot  = sum(obs_ha,  na.rm = TRUE),
    pred_plot = sum(pred_ha, na.rm = TRUE),
    error_pct = (pred_plot - obs_plot) / obs_plot * 100,
    .groups = "drop"
  )

resumen_parcela <- error_parcela %>%
  dplyr::group_by(Magnitud, Modelo) %>%
  dplyr::summarise(
    n_parcelas   = dplyr::n(),
    media_pct    = mean(error_pct, na.rm = TRUE),
    mediana_pct  = median(error_pct, na.rm = TRUE),
    sd_pct       = sd(error_pct, na.rm = TRUE),
    rmse_pct     = sqrt(mean(error_pct^2, na.rm = TRUE)),
    p_dentro_5   = mean(abs(error_pct) <= 5,  na.rm = TRUE) * 100,  # % parcelas con |err|<=5%
    .groups = "drop"
  )

# (c) POR PIE: solo como diagnóstico (los pies pequeños inflan el %)
resumen_pie <- error_long %>%
  dplyr::group_by(Magnitud, Modelo) %>%
  dplyr::summarise(
    media_pct = mean(error_pct, na.rm = TRUE),
    mae_pct   = mean(abs(error_pct), na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================================
#  BLOQUE ECONÓMICO — desviación ABSOLUTA en €/ha por producto
#  (el % económico = % físico; lo informativo aquí es el € que te cuesta)
#  Productos SEPARADOS (no se suman: madera y pellet son destinos alternativos).
# ============================================================================
productos <- c(eur_madera = "Madera (€/ha)",
               eur_pellet = "Pellet/Biomasa (€/ha)",
               eur_co2    = "CO₂ (€/ha)")

eur_long <- purrr::map_dfr(pred_h, function(hc) {
  purrr::map_dfr(names(productos), function(pr) {
    obs <- vbc[[paste0(pr, "_h_obs")]] * vbc$expan
    prd <- vbc[[paste0(pr, "_", hc)]]  * vbc$expan
    data.frame(PLOT_ID = vbc$PLOT_ID, INVENTORY_ID = vbc$INVENTORY_ID,
               Modelo = modelo_labels[[hc]], Producto = productos[[pr]],
               obs_ha = obs, pred_ha = prd)
  })
})

# Por parcela: € observados, € predichos y desviación €/ha
eur_parcela <- eur_long %>%
  dplyr::group_by(Producto, Modelo, PLOT_ID, INVENTORY_ID) %>%
  dplyr::summarise(
    obs_plot  = sum(obs_ha,  na.rm = TRUE),
    pred_plot = sum(pred_ha, na.rm = TRUE),
    dif_eur_ha = pred_plot - obs_plot,
    .groups = "drop"
  )

# Resumen económico por modelo y producto
resumen_eur <- eur_parcela %>%
  dplyr::group_by(Producto, Modelo) %>%
  dplyr::summarise(
    media_dif_eur_ha   = mean(dif_eur_ha, na.rm = TRUE),   # sesgo medio en €/ha
    mediana_dif_eur_ha = median(dif_eur_ha, na.rm = TRUE),
    mae_eur_ha         = mean(abs(dif_eur_ha), na.rm = TRUE), # error medio absoluto €/ha
    sd_eur_ha          = sd(dif_eur_ha, na.rm = TRUE),
    .groups = "drop"
  )


# --- Guardar tablas --------------------------------------------------------
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(tabla_global,    file.path(out_dir, paste0(sp_id, "_fis_global.csv")),       row.names = FALSE)
write.csv(resumen_parcela, file.path(out_dir, paste0(sp_id, "_fis_por_parcela.csv")),  row.names = FALSE)
write.csv(resumen_pie,     file.path(out_dir, paste0(sp_id, "_fis_por_pie.csv")),      row.names = FALSE)
write.csv(resumen_eur,     file.path(out_dir, paste0(sp_id, "_eco_por_parcela.csv")),  row.names = FALSE)

cat("\n=== Sesgo agregado físico (%) ===\n");           print(as.data.frame(tabla_global),    digits = 4)
cat("\n=== Error por parcela (resumen, %) ===\n");       print(as.data.frame(resumen_parcela), digits = 4)
cat("\n=== Desviación económica por parcela (€/ha) ===\n"); print(as.data.frame(resumen_eur),  digits = 4)
if (!has_expan) cat("\n[AVISO] Sin factor de expansión: los € NO son €/ha reales.\n")


# ============================================================================
#  GRÁFICOS
# ============================================================================
tema <- ggplot2::theme_minimal() +
  ggplot2::theme(plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"),
                 legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

# G1: sesgo agregado físico (%) — barra por magnitud
plot_global <- ggplot2::ggplot(tabla_global,
                               ggplot2::aes(Modelo, sesgo_pct, fill = Modelo)) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
  ggplot2::facet_wrap(~Magnitud, scales = "free_y") +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::labs(title = "Sesgo agregado en volumen / biomasa / carbono",
                subtitle = paste0(sp_name, " — % sobre el total (altura observada = referencia)"),
                x = NULL, y = "Sesgo agregado (%)") + tema

# G2: distribución del error POR PARCELA (%) — el gráfico clave
plot_parcela <- ggplot2::ggplot(error_parcela,
                                ggplot2::aes(Modelo, error_pct, fill = Modelo)) +
  ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  ggplot2::facet_wrap(~Magnitud, scales = "free_y") +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::labs(title = "Error por parcela en volumen / biomasa / carbono",
                subtitle = paste0(sp_name, " — una observación por parcela (error % en €/ha)"),
                x = NULL, y = "Error por parcela (%)") + tema

# G3: desviación económica €/ha por parcela y producto
plot_eur <- ggplot2::ggplot(eur_parcela,
                            ggplot2::aes(Modelo, dif_eur_ha, fill = Modelo)) +
  ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  ggplot2::facet_wrap(~Producto, scales = "free_y") +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::labs(title = "Desviación económica por parcela",
                subtitle = paste0(sp_name, " — €/ha (predicho − observado); productos NO aditivos"),
                x = NULL, y = "Desviación (€/ha)") + tema

ggplot2::ggsave(file.path(out_dir, paste0(sp_id, "_fis_global.png")),  plot_global,  dpi = 300, width = 11, height = 5)
ggplot2::ggsave(file.path(out_dir, paste0(sp_id, "_fis_parcela.png")), plot_parcela, dpi = 300, width = 11, height = 5)
ggplot2::ggsave(file.path(out_dir, paste0(sp_id, "_eco_parcela.png")), plot_eur,     dpi = 300, width = 11, height = 5)

cat("\nListo. Tablas y gráficos en: ", out_dir, "\n", sep = "")

