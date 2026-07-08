# Diccionario de datos / Data Dictionary

> 🌐 Documento bilingüe. Cada sección incluye la versión en español y en inglés. Bilingual document. Each section includes both the Spanish and English version.

Basado en el **Anexo B** del TFG (estructura de la base de datos y diccionario de variables). Based on **Annex B** of the thesis (database structure and variable dictionary).

------------------------------------------------------------------------

## 1. Estructura de la base de datos / Database structure

**ES.** La base de datos consolidada se organiza en dos niveles de agregación jerárquicamente relacionados: el nivel superior corresponde a la **parcela** y el inferior al **árbol individual**. Cada árbol queda vinculado a la parcela en la que fue medido, y cada parcela a su operativo de inventario de procedencia (Señalamiento, Parcela de contraste, o IFN ediciones 2, 3 y 4). La trazabilidad se garantiza mediante tres identificadores encadenados:

**EN.** The consolidated database is organised in two hierarchically related aggregation levels: the upper level is the **plot** and the lower level is the **individual tree**. Each tree is linked to the plot where it was measured, and each plot to its source inventory operation (Marking, Contrast plot, or NFI editions 2, 3 and 4). Traceability is guaranteed by three chained identifiers:

```         
INVENTORY_ID  ──▶  PLOT_ID  ──▶  TREE_ID
```

-   `INVENTORY_ID` — identifica el inventario de origen (uno a muchas parcelas) / identifies the source inventory (one-to-many plots).
-   `PLOT_ID` — identifica la parcela (una a muchos árboles) / identifies the plot (one-to-many trees).
-   `TREE_ID` — identifica el pie individual / identifies the individual tree.

Las variables a nivel de parcela (densidad, área basimétrica, altura dominante, etc.) se calculan por agregación de los pies de cada parcela y se propagan al nivel de árbol a través de `PLOT_ID` cuando el ajuste de los modelos extendidos lo requiere. / Plot-level variables (density, basal area, dominant height, etc.) are computed by aggregating the trees in each plot and propagated to the tree level through `PLOT_ID` when the extended models require it.

------------------------------------------------------------------------

## 2. Matriz de disponibilidad por fuente / Per-source availability matrix

**Leyenda / Legend:** `M` = medida en campo / field-measured · `D` = derivada por cálculo / derived by computation · `C` = obtenida de cartografía / obtained from cartography · `—` = no disponible / not available.

| Variable / grupo · Variable / group | JCyL–Ord. | JCyL–Señalam. | IFN (2–4) |
|----|:--:|:--:|:--:|
| Diámetro normal (dbh) · DBH | M | M | M |
| Altura total (h) · Total height | M | M | M |
| Clase de calidad de fuste · Stem quality class | M | M | M |
| Factor de expansión · Expansion factor | D | D | D |
| Variables de masa de parcela (N, dg, G, Ho, Do, SDI) · Stand-level variables | D | D | D |
| Índice de competencia (BAL) · Competition index | D | D | D |
| Altitud / coordenadas · Altitude / coordinates | C | C | C |
| Variables de valoración (VCC, biomasa, C, €) · Valuation variables | D | D | D |

> **ES.** Los datos de señalamiento se usan solo al ajustar y seleccionar el modelo base. **EN.** The data from the sampling methodology are used only to fit and select the best base model.

------------------------------------------------------------------------

## 3. Diccionario de variables / Variable dictionary

**Origen / Origin:** `Campo/Field` = medida en campo / field-measured · `Derivada/Derived` = calculada / computed · `Cartografía/Cartography` = de fuentes cartográficas / from cartographic sources.

### 3.1. Identificadores y trazabilidad / Identifiers and traceability

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Origen · Origin | Observaciones · Notes |
|----|----|----|----|----|----|
| `INVENTORY_ID` | Identificador de inventario de procedencia · Source inventory ID | — | Categórica · Categorical | Asignado · Assigned | Valores: JCYLORD, JCYLSEN, IFN… |
| `PLOT_ID` | Identificador único de parcela · Unique plot ID | — | Categórica · Categorical | Asignado · Assigned | Clave inventario–parcela · Inventory–plot key |
| `TREE_ID` | Identificador único de árbol · Unique tree ID | — | Categórica · Categorical | Asignado · Assigned | Clave inventario–parcela–árbol · Inventory–plot–tree key |
| `speciesname` | Especie · Species | — | Categórica · Categorical | Campo · Field | P. sylvestris, P. nigra, P. pinaster, Q. pyrenaica |
| `sp` | Código de especie · Species code | — | Continua · Continuous | Campo · Field | 21, 25, 26, 43 |

### 3.2. Variables medidas en campo – nivel de árbol / Field-measured variables – tree level

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Observaciones · Notes |
|----|----|----|----|----|
| `dbh` | Diámetro normal a 1,3 m · DBH at 1.3 m | cm | Continua · Continuous | — |
| `h` | Altura total · Total height | m | Continua · Continuous | Hipsómetro (Vertex); submuestra en JCyL · subsample in JCyL |
| `shape` | Clase de calidad del fuste · Stem quality class | — | Continua · Continuous | Clasificación cualitativa · Qualitative classification |
| `dead` | Vivo o muerto · Alive or dead | \% | Binaria · Binary | Herencia del IFN · Inherited from NFI |

### 3.3. Variables derivadas – nivel de árbol / Derived variables – tree level

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Observaciones · Notes |
|----|----|----|----|----|
| `expan` | Factor de expansión · Expansion factor | ha⁻¹ | Continua · Continuous | En el Excel de entrada · In the input Excel |
| `circumference` | Circunferencia normal (π·dbh) · Girth (π·dbh) | cm | Continua · Continuous | — |
| `hd_ratio` | Coeficiente de esbeltez h/dbh · Slenderness ratio | adim. · dimensionless | Continua · Continuous | Requiere h medida · Requires measured h |
| `g` | Área basimétrica individual · Individual basal area | m² | Continua · Continuous | — |
| `g_ha` | Área basimétrica individual extrapolada a ha · Individual basal area per ha | m² | Continua · Continuous | — |
| `slenderness` | Coeficiente de esbeltez · Slenderness | \% | Continua · Continuous | h/dbh |
| `BAL` | Área basimétrica de árboles mayores · Basal area of larger trees | m² ha⁻¹ | Continua · Continuous | Índice de competencia · Competition index |

### 3.4. Variables de masa – nivel de parcela / Stand-level variables – plot level

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Origen · Origin | Observaciones · Notes |
|----|----|----|----|----|----|
| `N` / `N075...` | Densidad total / por clase diamétrica · Total / per diameter-class density | pies ha⁻¹ · stems ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `dbhmin/max/mean` | Diámetro normal mín/máx/medio de parcela · Plot min/max/mean DBH | cm | Continua · Continuous | Derivada · Derived | — |
| `hmin/max/mean` | Altura mín/máx/media de parcela · Plot min/max/mean height | cm | Continua · Continuous | Derivada · Derived | — |
| `gmin/max/mean` | Área basimétrica mín/máx/media de parcela · Plot min/max/mean basal area | cm | Continua · Continuous | Derivada · Derived | — |
| `dg` | Diámetro medio cuadrático · Quadratic mean diameter | cm | Continua · Continuous | Derivada · Derived | — |
| `G` | Área basimétrica de la parcela · Plot basal area | m² ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `Ho` | Altura dominante (media de los 100 pies más gruesos ha⁻¹) · Dominant height (mean of 100 thickest stems ha⁻¹) | m | Continua · Continuous | Derivada · Derived | Variable de mayor coste de medición · Highest measurement cost |
| `Do` | Diámetro dominante (análogo a Ho) · Dominant diameter (analogous to Ho) | cm | Continua · Continuous | Derivada · Derived | — |
| `sp1/2/3` | Código de especie principal y acompañantes · Main and accompanying species codes | — | Continua · Continuous | Campo · Field | — |
| `Gsp1/2/3` | Área basimétrica de cada especie · Basal area per species | m² ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `Nsp1/2/3` | Densidad de cada especie · Density per species | pies ha⁻¹ · stems ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `Galive/Gdead` | Área basimétrica de pies vivos y muertos · Basal area of live/dead stems | m² ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `Nalive/Ndead` | Densidad de pies vivos y muertos · Density of live/dead stems | pies ha⁻¹ · stems ha⁻¹ | Continua · Continuous | Derivada · Derived | — |
| `slenderness` | Coeficiente de esbeltez de parcela · Plot slenderness | \% | Continua · Continuous | Derivada · Derived | hmean/dbhmean |
| `dominantslenderness` | Esbeltez dominante · Dominant slenderness | \% | Continua · Continuous | Derivada · Derived | Ho/Do |
| `SDI` | Índice de densidad de Reineke · Reineke Stand Density Index | adim. · dimensionless | Continua · Continuous | Derivada · Derived | r = 1,605 por defecto · by default |
| `S` | Índice de Hart-Becking (marco real) · Hart-Becking index (square layout) | — | Continua · Continuous | Derivada · Derived | No usada · Not used |
| `S_staggered` | Índice de Hart-Becking (tresbolillo) · Hart-Becking index (staggered layout) | — | Continua · Continuous | Derivada · Derived | No usada · Not used |

### 3.5. Variables ambientales y espaciales / Environmental and spatial variables

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Origen · Origin | Observaciones · Notes |
|----|----|----|----|----|----|
| `ALT` | Altitud · Altitude | m s.n.m. · m a.s.l. | Continua · Continuous | Cartografía · Cartography | MDE / IFN |

### 3.6. Variables derivadas para la valoración / Derived valuation variables

| Código · Code | Descripción · Description | Unidad · Unit | Tipo · Type | Origen · Origin | Observaciones · Notes |
|----|----|----|----|----|----|
| `VCC` | Volumen con corteza · Over-bark volume | m³ ha⁻¹ | Continua · Continuous | Derivada · Derived | Ec. de cubicación del IFN; en Q. pyrenaica se emplean ecuaciones de leñas sin h · IFN volume equations; for Q. pyrenaica, firewood equations without h |
| `biomasa` | Biomasa aérea total · Total aboveground biomass | kg | Continua · Continuous | Derivada · Derived | Ecuaciones de biomasa · Biomass equations |
| `carbono` | Carbono fijado · Fixed carbon | kg C | Continua · Continuous | Derivada · Derived | A partir de la biomasa · Derived from biomass |

------------------------------------------------------------------------

## 4. Códigos de especie / Species codes

| `sp` | Especie · Species          | Nombre común · Common name             |
|:----:|----------------------------|----------------------------------------|
|  21  | *Pinus sylvestris* L.      | Pino silvestre · Scots pine            |
|  25  | *Pinus nigra* L.           | Pino laricio / salgareño · Black pine  |
|  26  | *Pinus pinaster* Ait.      | Pino resinero / negral · Maritime pine |
|  43  | *Quercus pyrenaica* Willd. | Rebollo / melojo · Pyrenean oak        |
