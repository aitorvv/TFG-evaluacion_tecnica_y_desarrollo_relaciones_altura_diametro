library(tidyverse)
library(ggplot2)
library(psych)
library(openxlsx)
library(minpack.lm)

# Cargar las cuatro especies y combinarlas
species_ids <- c(26, 21, 23, 24)  # ajusta los IDs si son distintos
sp_names <- c("italic('Pinus pinaster')", "italic('Pinus sylvestris')", "italic('Pinus nigra')", "italic('Quercus pyrenaica')")

trees_all <- trees %>%   # usa el dataframe completo sin filtrar por especie
  filter(species %in% species_ids, dead == 0) %>%
  mutate(sp_name = factor(species, levels = species_ids, labels = sp_names))

# Gráfico combinado de altura
ggplot(trees_all, aes(x = "", y = h)) +
  geom_violin(fill = "lightgreen", color = "darkgreen", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = NA) +
  expand_limits(y = 0) +
  facet_wrap(~ sp_name, labeller = label_parsed) +
  labs(
    title = "Distribución de la altura total",
    x = "Conjunto de datos",
    y = "h (m)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/home/alicia/Uni/TFG/forR/Outputs/Violin_h_4especies.png", dpi = 300, width = 20, height = 14, unit = "cm")

# Gráfico combinado de diámetro
ggplot(trees_all, aes(x = "", y = dbh)) +
  geom_violin(fill = "lightblue", color = "darkblue", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = NA) +
  expand_limits(y = 0) +
  facet_wrap(~ sp_name, labeller = label_parsed) +
  labs(
    title = "Distribución del diámetro normal",
    x = "Conjunto de datos",
    y = "dbh (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/home/alicia/Uni/TFG/forR/Outputs/Violin_dbh_4especies.png", dpi = 300, width = 20, height = 14, unit = "cm")

