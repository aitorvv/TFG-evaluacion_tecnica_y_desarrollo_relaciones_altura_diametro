# Technical evaluation and development of height–diameter relationships for merchantable volume estimation

> 🌐 **Language:** **English** (available also in [Español](README.md))

Height–diameter (*h–d*) models for three conifers (*Pinus sylvestris* L., *Pinus nigra* L., *Pinus pinaster* Ait.) and one broadleaf (*Quercus pyrenaica* Willd.) in the north of the province of Palencia and neighbouring districts of León (Almanza, Cea, Valderrueda and Villazanzo of Valderaduey), and evaluation of error propagation to volume, biomass, carbon and economic value.

---

### 📊 **Try the models interactively!**
> 💡 **Do you want to estimate tree heights without running any code?** We have built an interactive Excel calculator pre-loaded with the base and mixed models developed in this study. Simply input the diameter at breast height (*dbh*) and required stand variables to get instantaneous height predictions.
> 
> 👉 **[Click here to download the Tree Height Calculator (.xlsx)!](calculadora_alturas_TFG.xlsx)** 🟢

---

This repository contains the code, data and results of the Bachelor's Thesis (TFG) **«Technical evaluation and development of height-diameter relationships for merchantable volume estimation»**. It can be consulted in the repository of the University of Valladolid.

------------------------------------------------------------------------

## Table of contents

- [Technical evaluation and development of height–diameter relationships for merchantable volume estimation](#technical-evaluation-and-development-of-heightdiameter-relationships-for-merchantable-volume-estimation)
  - [Table of contents](#table-of-contents)
  - [Context and motivation {#context-and-motivation}](#context-and-motivation-context-and-motivation)
  - [Objectives {#objectives}](#objectives-objectives)
  - [Data {#data}](#data-data)
  - [Methodology {#methodology-pipeline}](#methodology-methodology-pipeline)
  - [Repository structure {#repository-structure}](#repository-structure-repository-structure)
    - [Interactive calculator](#interactive-calculator)
  - [Requirements and installation {#requirements-and-installation}](#requirements-and-installation-requirements-and-installation)
  - [How to reproduce the analysis {#how-to-reproduce-the-analysis}](#how-to-reproduce-the-analysis-how-to-reproduce-the-analysis)
    - [Interactive/Command Line approach](#interactivecommand-line-approach)
  - [Outputs](#outputs)
  - [Main results {#main-results}](#main-results-main-results)
  - [Data dictionary {#data-dictionary}](#data-dictionary-data-dictionary)
  - [How to cite {#how-to-cite}](#how-to-cite-how-to-cite)
  - [Authorship and credits {#authorship-and-credits}](#authorship-and-credits-authorship-and-credits)
  - [License](#license)

------------------------------------------------------------------------

## Context and motivation {#context-and-motivation}

The total height of a tree is a critical parameter in forest inventories, but measuring it directly in the field is costly and error-prone. Height–diameter (*h–d*) relationships allow it to be estimated from the diameter at breast height (*dbh*), which is much easier to measure. The problem: the literature for Spain is fragmented and does not cover all species or regions, which forces the use of generic models or the reparameterization of local equations with unknown errors.

That error does not stay in the height: **it propagates and amplifies** when calculating volume, biomass, carbon and, finally, the economic value of the forest. In the north of Palencia —a transition zone of pre-montane plain— there is a gap of specific models, and the model currently used by the Junta de Castilla y León (JCyL) shows systematic deviations detected by the Territorial Service itself.

This work develops and validates own *h–d* models for the area and **quantifies in euros per hectare** the impact of choosing one model or another.

## Objectives {#objectives}

-   Fit *h–d* models using *dbh* as the only predictor variable.
-   Fit extended *h–d* models incorporating stand variables to improve accuracy.
-   Compare the resulting models against existing alternatives (JCyL and the national generalized model) using statistical criteria.
-   Evaluate the errors assumed by each model in productive terms (volume, biomass, carbon) and economic terms (€/ha).

## Data {#data}

The data come from **three sources** with heterogeneous sampling protocols, harmonized into a single database:

| Source | Description | No. plots | Period |
|----|----|----|----|
| **JCyL – Management** | Forest Management Plan for the «Saldaña y Otros» Group of Forests (temporary LiDAR contrast plots) | 150 | 2023 |
| **JCyL – Marking** | Marking plots (variable methodology depending on report) | 239 | 2021–2025 |
| **IFN** | National Forest Inventory, editions IFN2, IFN3 and IFN4 (systematic 1×1 km grid) | 1364 | 1986–present |

The consolidated database is organized into **two hierarchical levels** linked by identifiers:

```         
INVENTORY_ID  ──▶  PLOT_ID  ──▶  TREE_ID
 (inventory)       (plot)         (tree)
```

The stand variables (density, basal area, dominant height, etc.) are calculated by aggregation at plot level and propagated to tree level via `PLOT_ID`.

> 📖 The complete description of each field is in [**`DATA_DICTIONARY.md`**](DATA_DICTIONARY.md) (Annex B of the TFG: structure, availability matrix by source and variable dictionary).

## Methodology {#methodology-pipeline}

The analysis is developed entirely in **R** and follows these phases:

0.  **Climographs** — Walter-Lieth climographs to characterize the climatic gradient of the area (`WorldClimExtractR`).
1.  **Dataset harmonization** — Integration and cleaning of the three sources: filtering of target species, removal of plots outside the area, assignment of unique identifiers and calculation of derived variables (basal area, slenderness, *dg*, *G*, *Ho*, *Do*, *SDI*, *BAL*…).
2.  **Fitting of 95 base *h–d* models** — Non-linear regression (`nlsLM`, `minpack.lm` package) of 95 equations from the international literature. Those that do not converge are discarded. Selection by R², RMSE, mean bias (MB) and AIC.
3.  **Extended models (NLME)** — The best base models are expanded with stand variables through non-linear mixed models (`nlme`), with `Inventory` as random effect. The best alternative is selected with and without dominant height (*Ho*).
4.  **Model comparison** — Random 75/25 split (training/validation). Five models are compared: local base, extended with *Ho*, extended without *Ho*, JCyL model (Schröder & Álvarez González, 2001) and national generalized model (Vázquez-Veloso et al., 2025). Metrics: R², RMSE, MB + observed vs. predicted plots and residual distribution.
5.  **Volume, biomass and carbon** — Calculation of over-bark volume (VCC) and firewood (VLE) with IFN4 equations, aboveground biomass (Ruiz-Peinado et al., 2011, 2012) and carbon (Montero et al., 2005), automated with the `silviculture` package.
6.  **Economic evaluation** — Translation of the error to €/ha using market prices for sawlog wood, firewood, pellet and carbon credits (Annex C).

## Repository structure {#repository-structure}

```         
.
├── README.md                  # Spanish version (ES)
├── README.en.md               # This file (EN)
├── DATA_DICTIONARY.md         # Variable dictionary (Annex B)
├── CITATION.cff               # Citation metadata for the repository
├── LICENSE                    # MIT license terms
├── calculadora_alturas_TFG.xlsx # Interactive Excel calculator of the TFG to estimate heights
│
├── Datos/
│   ├── Raw/                   # Original data by source (JCyL, IFN)
│   └── Processed/             # Consolidated and harmonized database
│
├── Scripts/                         # scripts/
│   └── aux/
│       ├── 2.0_hd_equations.r                 # The 95 base models from the literature
│       ├── one_model_to_rule_them_all.r       # To compare with the generalized model
│       └── functions/                         # complement to the silviculture library
│   ├── First_steps/
│       ├── 1_harmonize_initial_df.qmd         # Database harmonization script
│       ├── Modelo_sencillo.qmd                # Linear regression and statistical plots 
│       └── Modelos_sencillo_violin.R          # Statistical plots
│   ├── Base_model/                            
│       ├── Modelos95.qmd                      # Fitting loop for the 95 base models (nlsLM)
│       └── plot_best_model.r                  # Plots of the best model from the 95 base models fit (nlsLM)
│   ├── Extended_model/                        
│       ├── analisis_correlacion.r             # Correlation of stand variables
│       ├── modelo_extendido_pt1.qmd           # Fitting of non-linear mixed models (nlme)
│       └── modelo_extendido_pt2.R             # Plots of the best extended model (pt1)
│   └── Compare/
│       ├── compare_models.R                   # Validation of the developed models and comparison with two reference models
│       └── balance.R                          # Volume, biomass and carbon errors of each model, economic errors and plots 
```

### Interactive calculator

The [calculadora_alturas_TFG.xlsx](calculadora_alturas_TFG.xlsx) file is an interactive Excel tool designed to easily estimate individual tree heights by typing its diameter and other key parameters, automatically applying the models developed in this work.

## Requirements and installation {#requirements-and-installation}

-   **R** ≥ 4.x (the Bachelor's Thesis uses the R Core Team version, 2026).
-   Required R packages at the beginning of each script

## How to reproduce the analysis {#how-to-reproduce-the-analysis}

Run the scripts in order from the project root. Quarto documents (`.qmd`) mix R code with formatted narrative text, so they are typically run interactively cell-by-cell in an editor (like VS Code or RStudio) or rendered with the terminal/console. R scripts (`.R` or `.r`) can be executed directly.

### Interactive/Command Line approach

```r
# PHASE 1: Dataset harmonization and data cleaning (Quarto)
# Open the file in RStudio/VS Code and execute code cells, or render from the terminal:
# quarto render Scripts/First_steps/1_harmonize_initial_df.qmd
# Or from the R console: quarto::quarto_render("Scripts/First_steps/1_harmonize_initial_df.qmd")

# PHASE 2: Fitting of the 95 literature base models (Quarto)
# quarto render Scripts/Base_model/Modelos95.qmd

# PHASE 3: Correlation analysis of stand variables (R script)
source("Scripts/Extended_model/analisis_correlacion.r")

# PHASE 4: Fitting of NLME non-linear mixed models (Quarto)
# quarto render Scripts/Extended_model/modelo_extendido_pt1.qmd

# PHASE 5: 75/25 data split and comparison metrics (R script)
source("Scripts/Compare/compare_models.R")

# PHASE 6: Volume, biomass, carbon estimation and economic balance in €/ha (R script)
source("Scripts/Compare/balance.R")
```

## Outputs

Many files are generated as a result of the loops and are later not used. Therefore, the most important ones are summarized in the following section.\

1.  1_harmonize_initial_df.qmd: .csv files found in Datos/Processed

2.  Modelos95.qmd (where sp is the species code):

    2.1 `<sp>/Ajuste/fit_<model>.RData  (one per fitted model)`

    2.2 `<sp>/Residuales/residuals_<model>.RData -> df with h_pred and residuals`

    2.3 `<sp>/Modelos95_resultados_modelos.csv -> RMSE/R2/AIC/MB table of all models`

    2.4 `/balance_econ/top5_<sp>.csv -> the 5 best by RMSE/AIC`

3.  plot_best_model.r:

    3.1 1_ajuste.png

    3.2 2_observados_vs_predichos.png

    3.3 3_residuales_vs_predicciones.png

    3.4 4_residuales_vs_diametro.png

    3.5 5_distribucion_residuales.png

    3.6 6_qq_residuales.png

    3.7 bloque_1_ajuste_prediccion.png -\> this one is used in the document

    3.8 bloque_2_diagnostico_residuales.png

4.  modelo_extendido_pt1.qmd:

    4.1 `output/<sp>/nlme_hd_models_<model>_fe_<i>_re_<j>.rds -> each fitted model (one per combination)`

    4.2 `output/<sp>_nlme_models_stats.csv -> metrics (AIC/BIC/RMSE/MAE/bias...) of ALL combinations, sorted by AIC`

5.  modelo_extendido_pt2.qmd:

(in Graficos_Seleccion_Extendido and Graficos_Seleccion_Extendido_sin_Ho):

```         
5.1 mejor_modelo_metricas.csv

5.2 1_ajuste.png

5.3 1b_ajuste_curva_media.png

5.4 2_observados_vs_predichos.png

5.5 3_residuales_vs_predicciones.png

5.6 4_residuales_vs_diametro.png

5.7 5_distribucion_residuales.png

5.8 6_qq_residuales.png

5.9 bloque_1_ajuste_prediccion.png -> this one is used in the document

5.10 bloque_1b_ajuste_curva_media.png

5.11 bloque_2_diagnostico_residuales.png
```

Common outputs (at the end):

```         
5.12 <sp>_mejores_modelos.csv -> summary of the two best (with/without Ho)

5.13 <sp>_predicciones_comparacion.csv -> predictions with/without Ho joined by TREE_ID
```

6.  compare_models.R

    6.1 `<sp>_comparativa_modelos_split.csv -> R2/RMSE/Bias/MAE metrics of the 5 models`

    6.2 `<sp>_alturas_predichas_test.csv -> h_obs + pred_h1..pred_h5`

    6.3 `<sp>_parametros_ajustados.csv -> coefficients (long format)`

    6.4 `<sp>_parametros_ajustados_ancho.csv -> coefficients (wide format)`

    6.5 `<sp>\_comparativa_curvas_hd.png`

    6.6 `<sp>\_comparativa_obs_vs_pred.png -\> this one is used in the document`

    6.7 `<sp>\_residuales_vs_dbh.png`

    6.8 `<sp>\_comparativa_violin_residuales.png -\> this one is used in the document`

7.  balance.R: only the plot-level data from the .csv was used

    7.1 `<sp>\_fis_global.csv -\> aggregate physical bias (%)`

    7.2 `<sp>\_fis_por_parcela.csv -\> error per plot (mean/median/RMSE/% within +/-5%)`

    7.3 `<sp>\_fis_por_pie.csv -\> error per tree (diagnostic)`

    7.4 `<sp>\_eco_por_parcela.csv -\> economic deviation EUR/ha per product`

    7.5 `<sp>\_fis_global.png`

    7.6 `<sp>\_fis_parcela.png`

    7.7 `<sp>\_eco_parcela.png`

## Main results {#main-results}

-   After a **local refit**, the JCyL model reaches an accuracy similar to the alternatives developed here; its systematic deviations seem to be due to **failures in its reparameterization** (few data or poorly representative height samples), not to an intrinsic defect of the model.
-   The **national generalized model** offers the worst fit, with systematic overestimation (e.g. in *Q. pyrenaica*: MB ≈ +1.43 m; R² ≈ 0.05), evidencing the risk of applying national-scale models to local populations.
-   The **local models without *Ho*** sacrifice some accuracy in exchange for practical applicability: they are useful when **no height measurement is available**.
-   The economic analysis confirms that the choice of the *h–d* model acquires its **greatest significance in the final valuation** (€/ha): small height errors are notably amplified in volume, biomass, carbon and market value.

## Data dictionary {#data-dictionary}

See [**`DATA_DICTIONARY.md`**](DATA_DICTIONARY.md) for the detail of:

-   Database structure (two hierarchical levels).
-   Variable availability matrix by source (measured / derived / cartographic / not available).
-   Complete variable dictionary (code, description, unit, type and origin).

## How to cite {#how-to-cite}

If you use this code or the data, please cite the TFG:

> Díez Gómez, A. X. (2026). *Evaluación técnica y desarrollo de relaciones altura-diámetro para la estimación de volumen maderable* [Bachelor's Thesis]. Bachelor's Degree in Forestry and Natural Environment Engineering, Higher Technical School of Agricultural Engineering.

Advisors: Felipe Bravo Oviedo · Aitor Vázquez Veloso.

A complete citation metadata entry in BibTeX/CFF formats can be found in the [CITATION.cff](CITATION.cff) file of this repository.

## Authorship and credits {#authorship-and-credits}

-   **TFG author:** Alicia Xu Díez Gómez
-   **Advisor:** Felipe Bravo Oviedo
-   **Co-advisor:** Aitor Vázquez Veloso
-   **Data:** Junta de Castilla y León (JCyL) and National Forest Inventory (IFN – MITECO).

## License

This repository is licensed under the MIT License. For more details, see the [LICENSE](LICENSE) file.
