# --- 1. Cargar librerías necesarias ----
# Si no está instalada abajo derecha, packages
library(tidyverse)   # Para manipular datos y hacer gráficos
# library(ggplot2)     # Para los gráficos
library(psych)       # Para resumen de variables numéricas
library(openxlsx)
library(minpack.lm)

# --- 2. Leer el archivo CSV ----
datos <- read.xlsx("/media/aitor/WDE/iuFOR_trabajo/Repositorios/LINUX/simanfor/inventarios/ejemplos/modelos_arbol_individual-masas_puras/Ppinaster_SIbericoMeridional_IBERO_IFN_todo.xlsx",
                   sheet = 'PiesMayores', sep = ";")

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

# TODO: intenta hacer un gráfico de violín, representa la distribución de datos pero de otra manera y aporta más info

# Boxplot de altura
boxplot(datos$altura_total,
        main = "Caja y bigotes de Altura",
        ylab = "Altura (m)",
        col = "lightgreen",
        border = "darkgreen")

# TODO: para este gráfico hazlo con la librería ggplot, que quedará mejor, y que se vea el punto 0,0 del eje de coordenadas; tienes que poner las unidades en los ejes
# TODO: mira el código de github, tengo unos gráficos "tipo" y puedes pillar alguno de allí si quieres

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

# TODO: esta línea da error
modelo_huang <- nls(altura_total ~ huang_2000_I(a, b, c, dbh),
                    data = datos,
                    start = list(a = 40, b = 20, c = 1))
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

# TODO: este modelo no me funciona
modelo_lundqvist <- nls(altura_total ~ lundqvist_1989(a, b, c, dbh),
                        data = datos,
                        start = list(a = 60, b = 30, c = 1))
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
     xlab = "DBH (cm)", ylab = "Altura total (m)",
     pch = 19, col = "gray40")

lines(dbh_seq, pred_huang, col = "red", lwd = 2)
lines(dbh_seq, pred_wykoff, col = "blue", lwd = 2)
lines(dbh_seq, pred_lundqvist, col = "forestgreen", lwd = 2)

legend("bottomright",
       legend = c("Huang 2000 I", "Wykoff 1982 II", "Lundqvist 1989"),
       col = c("red", "blue", "forestgreen"),
       lwd = 2, bty = "n")

# ---7.5 Función para evaluar la calidad del ajuste de un modelo nls ----
# TODO: esto te lo hizo el chati, no? tenemos que darle una vuelta
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
# TODO: este bloque de información no funciona, es porque no te funcionaron antes los modelos y porque tienes nombres distintos
result_huang <- evaluar_modelo(modelo2, datos, "altura_total", "dbh")
result_wykoff <- evaluar_modelo(modelo3, datos, "altura_total", "dbh")
result_lundqvist <- evaluar_modelo(modelo4, datos, "altura_total", "dbh")

# --- Comparación de resultados ---
indicadores <- rbind(
  cbind(Modelo = "Huang (2000 I)", result_huang),
  cbind(Modelo = "Wykoff (1982 II)", result_wykoff),
  cbind(Modelo = "Lundqvist (1989)", result_lundqvist)
)

print(indicadores)


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
# TODO: esto mejor muévelo a donde están los modelos por no mezclar cosas, ya lo ordenaremos un poco
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

# --- 9 Cálculo de biomasa total por árbol y total del conjunto ----
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

# --- 10 Cálculo del carbono fijado en toneladas ----
Carbono_fijado <- (Biomasa_total*0.511)/1000
print(Carbono_fijado)


# --- 11. Precio de la madera ----
# TODO: deja apuntado de qué es este precio y de donde lo has sacado; lo mismo con las ecuaciones de biomasa y volumen, mejor dejar una cita y
# cuando vayas a escribir es más fácil volver a encontrar todo
precio_madera <- 12.7  # €/m³

# --- Cálculo de pérdidas económicas según los errores absolutos del volumen ---

# TODO: cuidado con esto, si calculas la diferencia en valor absoluto y dices que son las pérdidas... cómo sabes que el modelo está calculando más o menos de lo real? quita el absoluto
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
