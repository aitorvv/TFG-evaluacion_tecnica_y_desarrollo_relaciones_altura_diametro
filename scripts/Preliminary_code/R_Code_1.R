# --- 1. Cargar librerías necesarias ----
# Si no está instalada abajo derecha, packages
library(tidyverse)   # Para manipular datos y hacer gráficos
library(ggplot2)     # Para los gráficos
library(psych)       # Para resumen de variables numéricas
library(minpack.lm)

# --- 2. Leer el archivo CSV ----
datos <- read.csv("C:/Users/Alicia/Desktop/TFG/DatosBrutos/Ppinaster_SIbericoMeridional_IBERO_IFN_todo(PiesMayores).csv", header = TRUE, sep = ";", dec = ",")

# F1 para desplegar ayuda de funciones


# --- 3. Explorar los datos ----
# Ver las primeras y últimas filas para ver si no hay ningún error en los datos
head(datos)
tail(datos)

# Ver estructura de las variables (numéricas, factores, etc.)
str(datos)

# Resumen general
summary(datos)

# 4. Variables cualitativas ####

# ver qué especies distintas tenemos
unique(datos$especie)

# filtrar pino pinaster (26)
datos <- subset(datos, especie == 26)

# convertir variable a chr y factor
datos$especie <- as.character(datos$especie)
str(datos$especie)

datos$especie <- as.factor(datos$especie)
str(datos$especie)

# 5. Variables cuantitativas ####

# analisis estadistico
summary(datos[,c("dbh","altura_total")])

# Histograma altura
hist(datos$altura_total,
     main = "Histograma de Altura",
     xlab = "Altura (m)",
     col = "lightgreen",
     border = "darkgreen")

# Histograma de Diametro
hist(datos$dbh,
     main = "Histograma de Diámetro",
     xlab = "Diámetro (cm)",
     col = "lightblue",
     border = "darkblue")

#Gráfico de violín altura
ggplot(datos, aes(x = "", y = altura_total)) +
  geom_violin(fill = "lightgreen", color = "darkgreen", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = NA) +  # Añade caja dentro del violín
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Mostrar eje 0
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  expand_limits(x = 0, y = 0) +
  labs(
    title = "Distribución de la altura total de los árboles",
    x = "Conjunto de datos",
    y = "Altura total (m)"
  ) +
  theme_minimal(base_size = 14)

#Gráfico de violín diámetro
ggplot(datos, aes(x = "", y = dbh)) +
  geom_violin(fill = "lightblue", color = "darkblue", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = NA) +  # Añade caja dentro del violín
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Mostrar eje 0
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  expand_limits(x = 0, y = 0) +
  labs(
    title = "Distribución de la altura total de los árboles",
    x = "Conjunto de datos",
    y = "dbh (cm)"
  ) +
  theme_minimal(base_size = 14)


# Boxplot de altura
ggplot(datos, aes(x = "", y = altura_total)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", width = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +   # Línea en y=0
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +   # Línea en x=0
  expand_limits(x = 0, y = 0) +                                      # Que se vea (0,0)
  labs(
    title = "Distribución de la altura total de los árboles",
    x = "Conjunto de datos",
    y = "Altura total (m)"
  ) +
  theme_minimal(base_size = 14)

# Boxplot de diámetro
ggplot(datos, aes(x = "", y = dbh)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", width = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +   # Línea en y=0
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +   # Línea en x=0
  expand_limits(x = 0, y = 0) +                                      # Que se vea (0,0)
  labs(
    title = "Distribución de la altura total de los árboles",
    x = "Conjunto de datos",
    y = "Altura total (m)"
  ) +
  theme_minimal(base_size = 14)

# Gráfico de puntos DBH vs Altura
plot(datos$dbh, datos$altura_total,
     main = "Relación dbh vs Altura Total",
     xlab = "dbh ",
     ylab = "Altura Total",
     pch = 19,            # tipo de punto sólido
     col = "forestgreen") # color de los puntos

# Añadir línea de tendencia lineal
abline(lm(altura_total ~ dbh, data = datos), col = "red", lwd = 2)


# --- 5. Ajustar un modelo de altura-diámetro ---
# Supongamos que tus columnas se llaman "Altura" y "Diametro"
# Si tienen otros nombres, cámbialos aquí
modelo <- lm(altura_total ~ dbh, data = datos)

# Ver resumen del modelo
summary(modelo)

# --- 6. Graficar la relación altura-diámetro ----
ggplot(datos, aes(x = dbh, y = altura_total)) +
  geom_point(color = "forestgreen", size = 2, alpha = 0.6) +  # Puntos de los árboles
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +  # Línea del modelo
  labs(
    title = "Relación Altura-Diámetro",
    x = "Diámetro (cm)",
    y = "Altura (m)"
  ) +
  theme_minimal()

# --- 7. Revisar modelos existentes ----
# --- 7.1. Modelo Huang (2000 I) ----
huang_2000_I <- function(a, b, c, dbh) {
  altura_total <- a / (1 + b * dbh^(-c))
  return(altura_total)
}

modelo_huang <- nlsLM(altura_total ~ huang_2000_I(a, b, c, dbh),
                    data = datos,
                    start = list(a = 25, b = 10, c = 1),
                    control = nls.lm.control (maxiter = 500, ftol = 1e-10, ptol = 1e-10))

summary(modelo_huang)

# Predicciones Huang
dbh_seq <- seq(min(datos$dbh), max(datos$dbh), length.out = 100)
pred_huang <- predict(modelo_huang, newdata = data.frame(dbh = dbh_seq))


# --- 7.2. Modelo Wykoff (1982 II) ----
wykoff_1982_II <- function(a, b, dbh) {
  altura_total <- 1.3 + exp(a + (b / (dbh + 1)))
  return(altura_total)
}

modelo_wykoff <- nls(altura_total ~ wykoff_1982_II(a, b, dbh),
                     data = datos,
                     start = list(a = 3, b = -20))
summary(modelo_wykoff)

# Predicciones Wykoff
pred_wykoff <- predict(modelo_wykoff, newdata = data.frame(dbh = dbh_seq))


# --- 7.3. Modelo Lundqvist (1989) ----
lundqvist_1989 <- function(a, b, c, dbh) {
  altura_total <- 1.3 + a * exp(-b * dbh^(-c))
  return(altura_total)
}

modelo_lundqvist <- nlsLM(altura_total ~ lundqvist_1989(a, b, c, dbh),
                        data = datos,
                        start = list(a = 60, b = 30, c = 1),
                        control = nls.lm.control(maxiter = 1000))
summary(modelo_lundqvist)

# Predicciones Lundqvist
pred_lundqvist <- predict(modelo_lundqvist, newdata = data.frame(dbh = dbh_seq))


# --- 7.4. Gráfico comparativo ----
# TODO: la idea de este gráfico me gusta, mira a ver si consigues replicarlo con ggplot; si se te complica, lo dejamos así
# TODO: aquí tienes una posible plantilla, el último bloque/función: https://github.com/aitorvv/height-diameter_models_Spain/blob/main/scripts/1.0_hd_support_functions.r
# TODO: en total nos van a interesar 3 tipos de gráficos: altura vs dbh, residuales (errores de predicciones), y h observada vs h predicha; intenta hacer una gráfico tipo de cada y luego lo adaptamos para hacerlo con todos los modelos, aquí un código de ejemplo: https://github.com/aitorvv/height-diameter_models_Spain/blob/main/scripts/2.5_best_model_graphs.r
# TODO: otra cosa que falta, creo que la única, es calcular el AIC y seleccionar el mejor modelo según ese criterio


plot(datos$dbh, datos$altura_total,
     main = "Comparación de modelos altura–diámetro",
     xlab = "Diámetro normal (cm)", ylab = "Altura total (m)",
     pch = 19, col = "gray40")

lines(dbh_seq, pred_huang, col = "red", lwd = 2)
lines(dbh_seq, pred_wykoff, col = "blue", lwd = 2)
lines(dbh_seq, pred_lundqvist, col = "forestgreen", lwd = 2)

legend("bottomright",
       legend = c("Huang 2000 I", "Wykoff 1982 II", "Lundqvist 1989"),
       col = c("red", "blue", "forestgreen"),
       lwd = 2, bty = "n")

# ---7.5 Función para evaluar la calidad del ajuste de un modelo nls ----
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
result_wykoff <- evaluar_modelo(modelo_wykoff, datos, "altura_total", "dbh")
result_lundqvist <- evaluar_modelo(modelo_lundqvist, datos, "altura_total", "dbh")

# --- Comparación de resultados ---
indicadores <- rbind(
  cbind(Modelo = "Huang (2000 I)", result_huang),
  cbind(Modelo = "Wykoff (1982 II)", result_wykoff),
  cbind(Modelo = "Lundqvist (1989)", result_lundqvist)
)

print(indicadores)
# 7.6 Valores residuales ####
#El modelo se ajustará mejor a los datos según la forma de la nuve de puntos. La línaea del "cero"
#debería quedar en mitad de toda esa nube sin que esta haga forma de cono, o se incline hacia arriba/abajo

modelo <- modelo_huang   

# --- 1. Calcular residuales y predicciones ---
residuales <- residuals(modelo)
predicciones <- fitted(modelo)

# --- 2. Gráfico de residuales vs predicciones ---
plot(predicciones, residuales,
     main = "Residuales vs Predicciones",
     xlab = "Altura predicha (m)",
     ylab = "Residuales (m)",
     pch = 19, col = "forestgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

modelo <- modelo_wykoff   

# --- 1. Calcular residuales y predicciones ---
residuales <- residuals(modelo)
predicciones <- fitted(modelo)

# --- 2. Gráfico de residuales vs predicciones ---
plot(predicciones, residuales,
     main = "Residuales vs Predicciones",
     xlab = "Altura predicha (m)",
     ylab = "Residuales (m)",
     pch = 19, col = "forestgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

modelo <- modelo_lundqvist   

# --- 1. Calcular residuales y predicciones ---
residuales <- residuals(modelo)
predicciones <- fitted(modelo)

# --- 2. Gráfico de residuales vs predicciones ---
plot(predicciones, residuales,
     main = "Residuales vs Predicciones",
     xlab = "Altura predicha (m)",
     ylab = "Residuales (m)",
     pch = 19, col = "forestgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)


#8. Cálculo del volumen----
# --- 8.1. Parámetros de las ecuaciones ----
p <- 0.0005646
q <- 1.99348
r <- 0.82029

a <- -9.58000
b <- 0.6387868
c <- 0.0000454

# --- 8.2. Calcular VCC y VSC reales ----
datos$VCC_real <- p * (datos$dbh * 10)^q * (datos$altura_total)^r
datos$VSC_real <- a + b * datos$VCC_real + c * (datos$VCC_real)^2

# --- 8.3. Calcular alturas estimadas de cada modelo ----
datos$H_huang <- predict(modelo_huang, newdata = datos)
datos$H_wykoff <- predict(modelo_wykoff, newdata = datos)
datos$H_lundqvist <- predict(modelo_lundqvist, newdata = datos)

# --- 8.4. Calcular VCC y VSC estimados ----
## HUANG
datos$VCC_huang <- p * (datos$dbh * 10)^q * (datos$H_huang)^r
datos$VSC_huang <- a + b * datos$VCC_huang + c * (datos$VCC_huang)^2

## WYKOFF
datos$VCC_wykoff <- p * (datos$dbh * 10)^q * (datos$H_wykoff)^r
datos$VSC_wykoff <- a + b * datos$VCC_wykoff + c * (datos$VCC_wykoff)^2

## LUNDQVIST
datos$VCC_lundqvist <- p * (datos$dbh * 10)^q * (datos$H_lundqvist)^r
datos$VSC_lundqvist <- a + b * datos$VCC_lundqvist + c * (datos$VCC_lundqvist)^2

# --- 8.5. Calcular errores ----
## Errores absolutos
datos$error_abs_VCC_huang <- datos$VCC_huang - datos$VCC_real
datos$error_abs_VCC_wykoff <- datos$VCC_wykoff - datos$VCC_real
datos$error_abs_VCC_lundqvist <- datos$VCC_lundqvist - datos$VCC_real

datos$error_abs_VSC_huang <- datos$VSC_huang - datos$VSC_real
datos$error_abs_VSC_wykoff <- datos$VSC_wykoff - datos$VSC_real
datos$error_abs_VSC_lundqvist <- datos$VSC_lundqvist - datos$VSC_real

## Errores relativos (%)
datos$error_rel_VCC_huang <- (datos$error_abs_VCC_huang / datos$VCC_real) * 100
datos$error_rel_VCC_wykoff <- (datos$error_abs_VCC_wykoff / datos$VCC_real) * 100
datos$error_rel_VCC_lundqvist <- (datos$error_abs_VCC_lundqvist / datos$VCC_real) * 100

datos$error_rel_VSC_huang <- (datos$error_abs_VSC_huang / datos$VSC_real) * 100
datos$error_rel_VSC_wykoff <- (datos$error_abs_VSC_wykoff / datos$VSC_real) * 100
datos$error_rel_VSC_lundqvist <- (datos$error_abs_VSC_lundqvist / datos$VSC_real) * 100

# TODO: cuidado con esto, si calculas la diferencia en valor absoluto y dices que son las pérdidas... cómo sabes que el modelo está calculando más o menos de lo real? quita el absoluto


# --- 8.6. Tabla comparativa de errores medios ----
errores_promedio <- data.frame(
  Modelo = c("Huang (2000 I)", "Wykoff (1982 II)", "Lundqvist (1989)"),
  Error_abs_medio_VCC = c(
    mean(abs(datos$error_abs_VCC_huang), na.rm = TRUE),
    mean(abs(datos$error_abs_VCC_wykoff), na.rm = TRUE),
    mean(abs(datos$error_abs_VCC_lundqvist), na.rm = TRUE)
  ),
  Error_rel_medio_VCC = c(
    mean(abs(datos$error_rel_VCC_huang), na.rm = TRUE),
    mean(abs(datos$error_rel_VCC_wykoff), na.rm = TRUE),
    mean(abs(datos$error_rel_VCC_lundqvist), na.rm = TRUE)
  ),
  Error_abs_medio_VSC = c(
    mean(abs(datos$error_abs_VSC_huang), na.rm = TRUE),
    mean(abs(datos$error_abs_VSC_wykoff), na.rm = TRUE),
    mean(abs(datos$error_abs_VSC_lundqvist), na.rm = TRUE)
  ),
  Error_rel_medio_VSC = c(
    mean(abs(datos$error_rel_VSC_huang), na.rm = TRUE),
    mean(abs(datos$error_rel_VSC_wykoff), na.rm = TRUE),
    mean(abs(datos$error_rel_VSC_lundqvist), na.rm = TRUE)
  )
)

print(errores_promedio)

# --- 8.7. Gráficos comparativos ----
#Comparación VCC (volumen con corteza)
plot(datos$dbh, datos$VCC_real,
     pch = 19, col = "gray50",
     main = "Comparación de Volumen con Corteza (VCC)",
     xlab = "Diámetro normal (cm)", ylab = "VCC (dm³)")

lines(lowess(datos$dbh, datos$VCC_huang), col = "red", lwd = 2)
lines(lowess(datos$dbh, datos$VCC_wykoff), col = "blue", lwd = 2)
lines(lowess(datos$dbh, datos$VCC_lundqvist), col = "forestgreen", lwd = 2)

legend("topleft",
       legend = c("Real", "Huang 2000 I", "Wykoff 1982 II", "Lundqvist 1989"),
       col = c("gray40", "red", "blue", "forestgreen"),
       lwd = c(NA, 2, 2, 2),
       pch = c(19, NA, NA, NA),
       bty = "n")

#Comparación VSC (volumen sin corteza)
plot(datos$dbh, datos$VSC_real,
     pch = 19, col = "gray50",
     main = "Comparación de Volumen sin Corteza (VSC)",
     xlab = "Diámetro normal (cm)", ylab = "VSC (dm³)")

lines(lowess(datos$dbh, datos$VSC_huang), col = "red", lwd = 2)
lines(lowess(datos$dbh, datos$VSC_wykoff), col = "blue", lwd = 2)
lines(lowess(datos$dbh, datos$VSC_lundqvist), col = "forestgreen", lwd = 2)

legend("topleft",
       legend = c("Real", "Huang 2000 I", "Wykoff 1982 II", "Lundqvist 1989"),
       col = c("gray40", "red", "blue", "forestgreen"),
       lwd = c(NA, 2, 2, 2),
       pch = c(19, NA, NA, NA),
       bty = "n")

# --- 9. Índices de calidad de ajustes ----
# RMSE (Root Mean Square Error)
RMSE <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

# R² (Coeficiente de determinación) 

# MB (Mean Bias)
MB <- function(obs, pred) {
  mean(pred - obs, na.rm = TRUE)
}

# AIC ya lo devuelve el resumen del modelo NLS
# Ejemplo: AIC(modelo_huang)

# --- Ejemplo práctico para tus modelos ---

# Predicciones de cada modelo (ya las tienes calculadas)
pred_huang <- predict(modelo2)
pred_wykoff <- predict(modelo3)
pred_lundqvist <- predict(modelo4)

# Calcular indicadores
indicadores <- data.frame(
  Modelo = c("Huang (2000 I)", "Wykoff (1982 II)", "Lundqvist (1989)"),
  RMSE = c(RMSE(datos$altura_total, pred_huang),
           RMSE(datos$altura_total, pred_wykoff),
           RMSE(datos$altura_total, pred_lundqvist)),
  R2 = c(R2(datos$altura_total, pred_huang),
         R2(datos$altura_total, pred_wykoff),
         R2(datos$altura_total, pred_lundqvist)),
  MB = c(MB(datos$altura_total, pred_huang),
         MB(datos$altura_total, pred_wykoff),
         MB(datos$altura_total, pred_lundqvist)),
  AIC = c(AIC(modelo2),
          AIC(modelo3),
          AIC(modelo4))
)

# Mostrar tabla comparativa
print(indicadores)

# --- 10 Cálculo de biomasa total por árbol y total del conjunto ----
# 'dbh' (en cm), 'altura_total' (en m) y W (en kg de peso seco)

# 1 Ecuaciones de biomasa (individuales)
datos$Ws <- 0.0278 * (datos$dbh ^ 2.115) * (datos$altura_total ^ 0.618)   # Fuste
datos$Wb7b27 <- 0.000381 * (datos$dbh ^ 3.141)                            # Ramas gruesas y medianas
datos$Wb2n <- 0.0129 * (datos$dbh ^ 2.320)                                # Ramas finas y acículas
datos$Wr <- 0.00444 * (datos$dbh ^ 2.804)                                 # Raíces

# 2 Biomasa total por árbol
datos$Biomasa_total_arbol <- datos$Ws + datos$Wb7b27 + datos$Wb2n + datos$Wr

# 3 Biomasa total del conjunto (suma de todos los árboles)
Biomasa_total <- sum(datos$Biomasa_total_arbol, na.rm = TRUE)

# 4 Mostrar resultados
print(Biomasa_total)

# 5 (Opcional) Vista rápida de los primeros resultados
head(datos[, c("dbh", "altura_total", "Ws", "Wb7b27", "Wb2n", "Wr", "Biomasa_total_arbol")])

# --- 11 Cálculo del carbono fijado en toneladas ----
Carbono_fijado <- (Biomasa_total*0.511)/1000
print(Carbono_fijado)

# Relación molecular CO2/C
relacion_CO2_C <- 44 / 12

# Cálculo del CO2 equivalente (en toneladas)
CO2_toneladas <- Carbono_fijado * relacion_CO2_C

# Mostrar resultado
cat("CO2 equivalente:", round(CO2_toneladas, 2), "toneladas\n")

# --- 11. Precio de la madera ----(Maderea)
precio_madera <- 12.7  # €/m³
#según el séptimo boletín trimestral de Madela de CyL, 2025 (https://datos.pfcyl.es/boletines?utm_medium=email&_hsenc=p2ANqtz--weV1s-D3ok_VJX9ogLIT3wsFUCpbG18HFWEwAGpAtXhkGKQW9Wmg2yVKvRk5TFZC94pJIHAAf0OoPtcTAITBCCYJtvu_8in23XrDjlNsYmsGWXLg&_hsmi=120299774&utm_content=120299774&utm_source=hs_email)
#En Ávila, el P.pinaster está en desenrollo a 80€/t

# --- Cálculo de pérdidas económicas según los errores absolutos del volumen ---

# Para el volumen con corteza (VCC)
datos$perdida_VCC_huang      <- abs(datos$error_abs_VCC_huang) * precio_madera
datos$perdida_VCC_wykoff     <- abs(datos$error_abs_VCC_wykoff) * precio_madera
datos$perdida_VCC_lundqvist  <- abs(datos$error_abs_VCC_lundqvist) * precio_madera

# Para el volumen sin corteza (VSC)
datos$perdida_VSC_huang      <- abs(datos$error_abs_VSC_huang) * precio_madera
datos$perdida_VSC_wykoff     <- abs(datos$error_abs_VSC_wykoff) * precio_madera
datos$perdida_VSC_lundqvist  <- abs(datos$error_abs_VSC_lundqvist) * precio_madera

# --- Pérdidas totales por modelo ---
perdidas_totales <- data.frame(
  Modelo = c("Huang (2000 I)", "Wykoff (1982 II)", "Lundqvist (1989)"),
  Perdida_total_VCC = c(
    sum(datos$perdida_VCC_huang, na.rm = TRUE),
    sum(datos$perdida_VCC_wykoff, na.rm = TRUE),
    sum(datos$perdida_VCC_lundqvist, na.rm = TRUE)
  ),
  Perdida_total_VSC = c(
    sum(datos$perdida_VSC_huang, na.rm = TRUE),
    sum(datos$perdida_VSC_wykoff, na.rm = TRUE),
    sum(datos$perdida_VSC_lundqvist, na.rm = TRUE)
  )
)

# Mostrar tabla de pérdidas
print(perdidas_totales)

# --- Pérdidas medias por árbol  ---
perdidas_totales$Perdida_media_VCC <- perdidas_totales$Perdida_total_VCC / nrow(datos)
perdidas_totales$Perdida_media_VSC <- perdidas_totales$Perdida_total_VSC / nrow(datos)

cat("\n💶 Pérdidas económicas medias por árbol (€):\n")
print(perdidas_totales[, c("Modelo", "Perdida_media_VCC", "Perdida_media_VSC")])


# --- 12. Precio del carbono en octubre de 2025 (SendeCO2) ----
precio_CO2 <- 78.24  # €/tCO2

# ---  Valor económico total del carbono almacenado ---
valor_carbono_total <- CO2_toneladas * precio_CO2  # en euros
print(valor_carbono_total)
  
