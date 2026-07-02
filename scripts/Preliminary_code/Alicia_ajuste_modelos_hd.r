library(tidyverse)
library(readxl)
library(minpack.lm)  # for nlsLM

df <- read_xlsx('/media/aitor/WDE/iuFOR_trabajo/Repositorios/LINUX/simanfor/inventarios/ejemplos/modelos_arbol_individual-masas_puras/Ppinaster_SIbericoMeridional_IBERO_IFN_todo.xlsx', sheet = "PiesMayores")



# Modelo lineal sencillo ----

m1 <- lm(df$altura_total ~ df$dbh)  # ajustar modelo lineal
summary(m1)  # ver resumen del modelo
df$h_m1 <- predict(object = m1, df = df$dbh)  # predecir alturas con el modelo lineal


# Modelo con una forma ya existente ----
# modelos: https://github.com/aitorvv/height-diameter_models_Spain/blob/main/scripts/2.0_hd_equations.r
# código: https://github.com/aitorvv/height-diameter_models_Spain/blob/main/scripts/2.1_hd_all_base_model_fit.r

# ecuación logística
logistic_model <- function(a, b, c, dbh) {
  altura_total <- a / (1 + b * exp(-c * dbh))
  return(altura_total)
}
ratkowsky_1986 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 / (1 + beta1^(-1) * dbh^(-beta2))
  return(h)
}

# create the formula for nls adapted to each model

# parámetros
start_params <- list(a = 1, b = 0.5, c = 0.1)
param_names <- names(start_params)

# convertir la ecuación original a fórmula
model_function <- logistic_model
formula_str <- paste0("altura_total ~ model_function(", paste(param_names, collapse = ", "), ", dbh)")
model_formula <- as.formula(formula_str)

# ajustar el modelo no lineal
m_logistico <- nlsLM(formula = model_formula, 
                     data = df, 
                     start = start_params)
summary(m_logistico)

# predicted values
df$h_logictico <- predict(object = m_logistico, 
                     newdata = df)


# Visualización de resultados ~ modelos ajustados ----
ggplot(df) +
  geom_point(aes(x = dbh, y = altura_total), color = 'gray') +
  geom_line(aes(x = dbh, y = h_m1), color = 'blue', size = 1) +
  geom_line(aes(x = dbh, y = h_logictico), color = 'red', size = 1) +
  labs(x = "Diámetro a la altura del pecho (cm)",
       y = "Altura total (m)",
       title = "Modelos de altura-diámetro para Pinus pinaster en el sur
 de la península ibérica") +
  theme_minimal() +
  theme(text = element_text(size = 16))


# Visualización de residuales ~ modelo lineal ----
df$resid_m1 <- df$altura_total - df$h_m1

ggplot(df) +
  geom_point(aes(x = dbh, y = resid_m1), color = 'blue') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Diámetro a la altura del pecho (cm)",
       y = "Residuos del modelo lineal",
       title = "Residuos del modelo lineal") +
  theme_minimal() +
  theme(text = element_text(size = 16))


# Visualización de residuoale -~ modelo logístico ---
df$resid_logistico <- df$altura_total - df$h_logictico

ggplot(df) +
  geom_point(aes(x = dbh, y = resid_logistico), color = 'blue') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Diámetro a la altura del pecho (cm)",
       y = "Residuos del modelo lineal",
       title = "Residuos del modelo lineal") +
  theme_minimal() +
  theme(text = element_text(size = 16))
