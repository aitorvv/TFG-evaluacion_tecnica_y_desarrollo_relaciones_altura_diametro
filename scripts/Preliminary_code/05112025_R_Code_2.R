# Este código es para el ajuste de los modelos añadiéndoles una variable de Masa que ayude a mejorar las constantes a,b o c que tenga la ecuación

library(readxl)
library (dplyr)
# Cargar la primera hoja del archivo
datos_Parcela <- read_excel("C:/Users/Alicia/Desktop/TFG/DatosBrutos/Ppinaster_SIbericoMeridional_IBERO_IFN_todo.xlsx", sheet = 1)
datos_combinados <- inner_join(datos, datos_Parcela, by = c("ID_Inventario", "ID_Parcela"))
datos_combinados <- rename(datos_combinados, G = Area_basimetrica)

#Con G
huang_2000_I_G <- function(a, b, c, d, dbh, G) {
  altura_total <- a / (1 + b * dbh^(-c)+d*G)
  return(altura_total)
}
modelo_huang_G <- nlsLM(altura_total ~ huang_2000_I_G(a, b, c, d, dbh, G),
                      data = datos_combinados,
                      start = list(a = 25, b = 10, c = 1, d= 5),
                      control = nls.lm.control (maxiter = 500, ftol = 1e-10, ptol = 1e-10))

summary(modelo_huang_G)

#Original
huang_2000_I <- function(a, b, c, dbh) {
  altura_total <- a / (1 + b * dbh^(-c))
  return(altura_total)
}

modelo_huang <- nlsLM(altura_total ~ huang_2000_I(a, b, c, dbh),
                      data = datos,
                      start = list(a = 25, b = 10, c = 1),
                      control = nls.lm.control (maxiter = 500, ftol = 1e-10, ptol = 1e-10))

summary(modelo_huang)

#Parámetros de calidad de ajuste entre los dos modelos antriores:
evaluar_modelo <- function(modelo, datos, variable_obs, variable_pred) {
  # variable_obs → "altura_total"
  # variable_pred →  "dbh"
  
  # Predicciones del modelo
  pred <- predict(modelo, newdata = datos)
  obs <- datos[[variable_obs]]
  
  # Indicadores
  RMSE <- sqrt(mean((obs - pred)^2, na.rm = TRUE))                  # Raíz del error cuadrático medio
  R2 <- 1 - (sum((obs - pred)^2, na.rm = TRUE) /                   # Pseudo R2
               sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  AIC_value <- AIC(modelo)                                          # Criterio de Akaike
  MB <- mean(obs - pred, na.rm = TRUE)                              # Sesgo medio
  
  # Resultados como data frame
  resultados <- data.frame(
    RMSE = RMSE,
    R2 = R2,
    AIC = AIC_value,
    MB = MB
  )
  return(resultados)
}

# --- Ejemplo de uso con tus modelos ---
result_huang <- evaluar_modelo(modelo_huang, datos, "altura_total", "dbh")
result_huang_G <- evaluar_modelo(modelo_huang_G, datos, "altura_total", "dbh")

# --- Comparación de resultados ---
indicadores <- rbind(
  cbind(Modelo = "Huang (2000 I)", result_huang),
  cbind(Modelo = "Huang (2000 I, G)", result_huang_G)
)

print(indicadores)
