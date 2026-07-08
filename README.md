# Evaluación técnica y desarrollo de relaciones altura–diámetro para la estimación de volumen maderable

> 🌐 **Idioma:** **Español** (disponible también en [English](README.en.md))

Modelos altura–diámetro (*h–d*) para tres coníferas (*Pinus sylvestris* L., *Pinus nigra* L., *Pinus pinaster* Ait.) y una frondosa (*Quercus pyrenaica* Willd.) en el norte de la provincia de Palencia y comarcas colindantes de León (Almanza, Cea, Valderrueda y Villazanzo de Valderaduey), y evaluación de la propagación del error a volumen, biomasa, carbono y valor económico.

---

### 📊 **¡Prueba los modelos de forma interactiva!**
> 💡 **¿Quieres estimar alturas de árboles sin necesidad de ejecutar código?** Hemos preparado una herramienta Excel interactiva estructurada con los modelos mixtos y base resultantes del TFG. Solo tienes que introducir el diámetro normal (*dbh*) y las variables de masa requeridas para obtener las predicciones automáticas.
> 
> 👉 **[¡Haz clic aquí para descargar la Calculadora de Alturas (.xlsx)!](calculadora_alturas_TFG.xlsx)** 🟢

---

Este repositorio contiene el código, los datos y los resultados del Trabajo de Fin de Grado (TFG) **«Evaluación técnica y desarrollo de relaciones altura-diámetro para la estimación de volumen maderable»**. Se puede consultar en el repositorio de la Universidad de Valladolid.

------------------------------------------------------------------------

## Tabla de contenidos

- [Evaluación técnica y desarrollo de relaciones altura–diámetro para la estimación de volumen maderable](#evaluación-técnica-y-desarrollo-de-relaciones-alturadiámetro-para-la-estimación-de-volumen-maderable)
    - [📊 **¡Prueba los modelos de forma interactiva!**](#-prueba-los-modelos-de-forma-interactiva)
  - [Tabla de contenidos](#tabla-de-contenidos)
  - [Contexto y motivación {#contexto-y-motivación}](#contexto-y-motivación-contexto-y-motivación)
  - [Objetivos {#objetivos}](#objetivos-objetivos)
  - [Datos {#datos}](#datos-datos)
  - [Metodología {#metodología-pipeline}](#metodología-metodología-pipeline)
  - [Estructura del repositorio {#estructura-del-repositorio}](#estructura-del-repositorio-estructura-del-repositorio)
    - [Calculadora interactiva](#calculadora-interactiva)
  - [Requisitos e instalación {#requisitos-e-instalación}](#requisitos-e-instalación-requisitos-e-instalación)
  - [Cómo reproducir el análisis {#cómo-reproducir-el-análisis}](#cómo-reproducir-el-análisis-cómo-reproducir-el-análisis)
    - [Vía interactiva o consola de comandos](#vía-interactiva-o-consola-de-comandos)
  - [Outputs](#outputs)
  - [Resultados principales {#resultados-principales}](#resultados-principales-resultados-principales)
  - [Diccionario de datos {#diccionario-de-datos}](#diccionario-de-datos-diccionario-de-datos)
  - [Cómo citar {#cómo-citar}](#cómo-citar-cómo-citar)
  - [Autoría y créditos {#autoría-y-créditos}](#autoría-y-créditos-autoría-y-créditos)
  - [Licencia](#licencia)

------------------------------------------------------------------------

## Contexto y motivación {#contexto-y-motivación}

La altura total de un árbol es un parámetro crítico en los inventarios forestales, pero medirla directamente en campo es costoso y propenso a errores. Las relaciones altura–diámetro (*h–d*) permiten estimarla a partir del diámetro normal (*dbh*), mucho más fácil de medir. El problema: la literatura para España está fragmentada y no cubre todas las especies ni regiones, lo que obliga a usar modelos genéricos o a reparametrizar ecuaciones locales con errores desconocidos.

Ese error no se queda en la altura: **se propaga y se amplifica** al calcular volumen, biomasa, carbono y, finalmente, el valor económico del monte. En el norte de Palencia —zona de transición de llanura premontañosa— existe un vacío de modelos específicos, y el modelo actualmente usado por la Junta de Castilla y León (JCyL) presenta desviaciones sistemáticas detectadas por el propio Servicio Territorial.

Este trabajo desarrolla y valida modelos *h–d* propios para la zona y **cuantifica en euros por hectárea** el impacto de elegir un modelo u otro.

## Objetivos {#objetivos}

-   Ajustar modelos *h–d* usando el *dbh* como única variable predictora.
-   Ajustar modelos *h–d* extendidos incorporando variables de masa para mejorar la precisión.
-   Comparar los modelos resultantes frente a las alternativas existentes (JCyL y modelo generalizado nacional) mediante criterios estadísticos.
-   Evaluar los errores asumidos por cada modelo en términos productivos (volumen, biomasa, carbono) y económicos (€/ha).

## Datos {#datos}

Los datos proceden de **tres orígenes** con protocolos de muestreo heterogéneos, armonizados en una única base de datos:

| Fuente | Descripción | Nº parcelas | Periodo |
|------------------|------------------|------------------|------------------|
| **JCyL – Ordenación** | Plan Dasocrático Grupo de Montes «Saldaña y Otros» (parcelas temporales de contraste LiDAR) | 150 | 2023 |
| **JCyL – Señalamiento** | Parcelas de señalamiento (metodología variable según informe) | 239 | 2021–2025 |
| **IFN** | Inventario Forestal Nacional, ediciones IFN2, IFN3 e IFN4 (malla sistemática 1×1 km) | 1364 | 1986–actualidad |

La base de datos consolidada se organiza en **dos niveles jerárquicos** encadenados por identificadores:

```         
INVENTORY_ID  ──▶  PLOT_ID  ──▶  TREE_ID
 (inventario)      (parcela)      (árbol)
```

Las variables de masa (densidad, área basimétrica, altura dominante, etc.) se calculan por agregación a nivel de parcela y se propagan al nivel de árbol vía `PLOT_ID`.

> 📖 La descripción completa de cada campo está en [**`DATA_DICTIONARY.md`**](DATA_DICTIONARY.md) (Anexo B del TFG: estructura, matriz de disponibilidad por fuente y diccionario de variables).

## Metodología {#metodología-pipeline}

El análisis se desarrolla íntegramente en **R** y sigue estas fases:

0.  **Climogramas** — Climogramas de Walter-Lieth para caracterizar el gradiente climático de la zona (`WorldClimExtractR`).
1.  **Armonización del dataset** — Integración y depuración de las tres fuentes: filtrado de especies objetivo, eliminación de parcelas fuera del área, asignación de identificadores únicos y cálculo de variables derivadas (área basimétrica, esbeltez, *dg*, *G*, *Ho*, *Do*, *SDI*, *BAL*…).
2.  **Ajuste de 95 modelos base *h–d*** — Regresión no lineal (`nlsLM`, paquete `minpack.lm`) de 95 ecuaciones de la literatura internacional. Los que no convergen se descartan. Selección por R², RMSE, sesgo medio (MB) y AIC.
3.  **Modelos extendidos (NLME)** — Los mejores modelos base se expanden con variables de masa mediante modelos mixtos no lineales (`nlme`), con `Inventario` como efecto aleatorio. Se selecciona la mejor alternativa con y sin altura dominante (*Ho*).
4.  **Comparación de modelos** — División aleatoria 75/25 (entrenamiento/validación). Se comparan cinco modelos: base local, extendido con *Ho*, extendido sin *Ho*, modelo JCyL (Schröder & Álvarez González, 2001) y modelo generalizado nacional (Vázquez-Veloso et al., 2025). Métricas: R², RMSE, MB + gráficos observado vs. predicho y distribución de residuales.
5.  **Volumen, biomasa y carbono** — Cálculo de volumen con corteza (VCC) y de leñas (VLE) con ecuaciones del IFN4, biomasa aérea (Ruiz-Peinado et al., 2011, 2012) y carbono (Montero et al., 2005), automatizado con el paquete `silviculture`.
6.  **Evaluación económica** — Traducción del error a €/ha usando precios de mercado de madera de sierra, leñas, pellet y créditos de carbono (Anexo C).

## Estructura del repositorio {#estructura-del-repositorio}

```         
.
├── README.md                  # Este archivo (ES)
├── README.en.md               # Versión en inglés
├── DATA_DICTIONARY.md         # Diccionario de variables (Anexo B)
├── CITATION.cff               # Metadatos de citación del repositorio
├── LICENSE                    # Términos de la licencia MIT
├── calculadora_alturas_TFG.xlsx # Calculadora Excel interactiva del TFG para estimar alturas
│
├── Datos/
│   ├── Raw/                   # Datos originales por fuente (JCyL, IFN)
│   └── Processed/             # Base de datos consolidada y armonizada
│
├── Scripts/                         # scripts/
│   └── aux/
│       ├── 2.0_hd_equations.r                 # Los 95 modelos base de la literatura
│       ├── one_model_to_rule_them_all.r       # Para comparar con el modelo generalizado
│       └── functions/                         # complementario de la librería silviculture
│   ├── First_steps/
│       ├── 1_harmonize_initial_df.qmd         # Script de armonización de la base de datos
│       ├── Modelo_sencillo.qmd                # Regresión lineal y gráficos estadísticos 
│       └── Modelos_sencillo_violin.R          # Gráficos estadísticos
│   ├── Base_model/                            
│       ├── Modelos95.qmd                      # Bucle de ajuste de los 95 modelos base (nlsLM)
│       └── plot_best_model.r                  # Gráficos del mejor modelo del ajuste de los 95 modelos base (nlsLM)
│   ├── Extended_model/                        
│       ├── analisis_correlacion.r             # Correlación de variables de masa
│       ├── modelo_extendido_pt1.qmd           # Ajuste de Modelos mixtos no lineales (nlme)
│       └── modelo_extendido_pt2.R             # Gráficos del mejor modelo extendido (pt1)
│   └── Compare/
│       ├── compare_models.R                   # Validación de los modelos desarrollados y comparación con dos modelos de referencia
│       └── balance.R                          # Errores de volumen, biomasa y carbono de cada modelo, errores económicos y gráficos 
```

### Calculadora interactiva

El archivo [calculadora_alturas_TFG.xlsx](calculadora_alturas_TFG.xlsx) es una herramienta en formato Excel que permite al usuario estimar la altura de un árbol introduciendo su diámetro y demás variables requeridas, aplicando de manera automática los modelos desarrollados en este trabajo.

## Requisitos e instalación {#requisitos-e-instalación}

-   **R** ≥ 4.x (el TFG usa la versión de R Core Team, 2026).
-   Paquetes de R al principio de cada script

## Cómo reproducir el análisis {#cómo-reproducir-el-análisis}

Ejecuta los scripts desde la raíz del proyecto en el siguiente orden. Los documentos Quarto (`.qmd`) mezclan código R y descripciones formateadas, por lo que se ejecutan interactivamente en un editor (como VS Code o RStudio) o se renderizan con la terminal. Los scripts de R (`.R` o `.r`) se ejecutan directamente.

### Vía interactiva o consola de comandos

```r
# FASE 1: Armonización y depuración de datos (Quarto)
# Puedes abrir el archivo en RStudio/VS Code y correr las celdas, o renderizarlo desde la terminal:
# quarto render Scripts/First_steps/1_harmonize_initial_df.qmd
# O desde la consola de R con: quarto::quarto_render("Scripts/First_steps/1_harmonize_initial_df.qmd")

# FASE 2: Ajuste de los 95 modelos base de la literatura (Quarto)
# quarto render Scripts/Base_model/Modelos95.qmd

# FASE 3: Análisis de correlación de variables de masa (R script)
source("Scripts/Extended_model/analisis_correlacion.r")

# FASE 4: Ajuste de modelos extendidos mixtos no lineales nlme (Quarto)
# quarto render Scripts/Extended_model/modelo_extendido_pt1.qmd

# FASE 5: División 75/25 y métricas de comparación (R script)
source("Scripts/Compare/compare_models.R")

# FASE 6: Estimaciones de volumen/biomasa/carbono y balance económico en €/ha (R script)
source("Scripts/Compare/balance.R")
```

## Outputs

Se generan muchos archivos resultado de los bucles que luego no se emplean. Por ello, se resumen los más importantes en el siguiete apartado.\

1.  1_harmonize_initial_df.qmd: Archivos .csv encontrados en Datos/Processed

2.  Modelos95.qmd (siendo sp el código de la especie):

    2.1 `<sp>/Ajuste/fit_<modelo>.RData  (uno por modelo ajustado)`

    2.2 `<sp>/Residuales/residuals_<modelo>.RData -> df con h_pred y residuals`

    2.3 `<sp>/Modelos95_resultados_modelos.csv -> tabla RMSE/R2/AIC/MB de todos los modelos`

    2.4 `/balance_econ/top5_<sp>.csv -> los 5 mejores por RMSE/AIC`

3.  plot_best_model.r:

    3.1 1_ajuste.png

    3.2 2_observados_vs_predichos.png

    3.3 3_residuales_vs_predicciones.png

    3.4 4_residuales_vs_diametro.png

    3.5 5_distribucion_residuales.png

    3.6 6_qq_residuales.png

    3.7 bloque_1_ajuste_prediccion.png -\> se usa este en el documento

    3.8 bloque_2_diagnostico_residuales.png

4.  modelo_extendido_pt1.qmd:

    4.1 `output/<sp>/nlme_hd_models_<modelo>_fe_<i>_re_<j>.rds -> cada modelo ajustado (uno por combinacion)`

    4.2 `output/<sp>_nlme_models_stats.csv -> metricas (AIC/BIC/RMSE/MAE/bias...) de TODAS las combinaciones, ordenado por AIC`

5.  modelo_extendido_pt2.qmd:

(en Graficos_Seleccion_Extendido y Graficos_Seleccion_Extendido_sin_Ho):

```         
5.1 mejor_modelo_metricas.csv

5.2 1_ajuste.png

5.3 1b_ajuste_curva_media.png

5.4 2_observados_vs_predichos.png

5.5 3_residuales_vs_predicciones.png

5.6 4_residuales_vs_diametro.png

5.7 5_distribucion_residuales.png

5.8 6_qq_residuales.png

5.9 bloque_1_ajuste_prediccion.png -> se usa este en el documento

5.10 bloque_1b_ajuste_curva_media.png

5.11 bloque_2_diagnostico_residuales.png
```

Outputs comunes (al final):

```         
5.12 <sp>_mejores_modelos.csv -> resumen de los dos mejores (con/sin Ho)

5.13 <sp>_predicciones_comparacion.csv -> predicciones con/sin Ho unidas por TREE_ID
```

6.  compare_models.R

    6.1 `<sp>_comparativa_modelos_split.csv -> metricas R2/RMSE/Sesgo/MAE de los 5 modelos`

    6.2 `<sp>_alturas_predichas_test.csv -> h_obs + pred_h1..pred_h5`

    6.3 `<sp>_parametros_ajustados.csv -> coeficientes (formato largo)`

    6.4 `<sp>_parametros_ajustados_ancho.csv -> coeficientes (formato ancho)`

    6.5 `<sp>\_comparativa_curvas_hd.png`

    6.6 `<sp>\_comparativa_obs_vs_pred.png -\> se usa este en el documento`

    6.7 `<sp>\_residuales_vs_dbh.png`

    6.8 `<sp>\_comparativa_violin_residuales.png -\> se usa este en el documento`

7.  balance.R: solo se usó los datos del .csv a nivel de parcela

    7.1 `<sp>\_fis_global.csv -\> sesgo agregado fisico (%)`

    7.2 `<sp>\_fis_por_parcela.csv -\> error por parcela (media/mediana/RMSE/% dentro de +/-5%)`

    7.3 `<sp>\_fis_por_pie.csv -\> error por pie (diagnostico)`

    7.4 `<sp>\_eco_por_parcela.csv -\> desviacion economica EUR/ha por producto`

    7.5 `<sp>\_fis_global.png`

    7.6 `<sp>\_fis_parcela.png`

    7.7 `<sp>\_eco_parcela.png`

## Resultados principales {#resultados-principales}

-   Tras un **reajuste local**, el modelo de la JCyL alcanza una precisión similar a las alternativas desarrolladas aquí; sus desviaciones sistemáticas parecen deberse a **fallos en su reparametrización** (pocos datos o muestras de altura poco representativas), no a un defecto intrínseco del modelo.
-   El **modelo generalizado nacional** ofrece el peor ajuste, con sobreestimación sistemática (p. ej. en *Q. pyrenaica*: MB ≈ +1,43 m; R² ≈ 0,05), evidenciando el riesgo de aplicar modelos de escala nacional sobre poblaciones locales.
-   Los **modelos locales sin *Ho*** sacrifican algo de precisión a cambio de aplicabilidad práctica: son útiles cuando **no se dispone de ninguna medición de altura**.
-   El análisis económico confirma que la elección del modelo *h–d* adquiere su **mayor trascendencia en la valoración final** (€/ha): errores de altura pequeños se amplifican notablemente en volumen, biomasa, carbono y valor de mercado.

## Diccionario de datos {#diccionario-de-datos}

Consulta [**`DATA_DICTIONARY.md`**](DATA_DICTIONARY.md) para el detalle de:

-   Estructura de la base de datos (dos niveles jerárquicos).
-   Matriz de disponibilidad de variables por fuente (medida / derivada / cartográfica / no disponible).
-   Diccionario completo de variables (código, descripción, unidad, tipo y origen).

## Cómo citar {#cómo-citar}

Si utilizas este código o los datos, por favor cita el TFG:

> Díez Gómez, A. X. (2026). *Evaluación técnica y desarrollo de relaciones altura-diámetro para la estimación de volumen maderable* [Trabajo de Fin de Grado]. Grado en Ingeniería Forestal y del Medio Natural, Escuela Técnica Superior de Ingenierías Agrarias.

Tutores: Felipe Bravo Oviedo · Aitor Vázquez Veloso.

También puedes encontrar los metadatos de citación listos en formato bibtex/CFF en el archivo [CITATION.cff](CITATION.cff) de este repositorio.

## Autoría y créditos {#autoría-y-créditos}

-   **Autora del TFG:** Alicia Xu Díez Gómez
-   **Tutor:** Felipe Bravo Oviedo
-   **Cotutor:** Aitor Vázquez Veloso
-   **Datos:** Junta de Castilla y León (JCyL) e Inventario Forestal Nacional (IFN – MITECO).

## Licencia

Este repositorio está bajo la Licencia MIT. Para más detalles consulta el archivo [LICENSE](LICENSE).
