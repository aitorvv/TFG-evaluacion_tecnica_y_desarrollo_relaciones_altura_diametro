
# 1: Packages and Data 

library(ggplot2)
library(GGally)
library(dplyr)

# En modelo extendido 
# Ese tree_data se ha exportado de la unión de harmonized_trees y harmonized_plots con el cálculo del BAL y de ALT.
tree_data <- read.csv('/home/alicia/Uni/TFG/Compare/data/tree_data.csv')
vars <- select(tree_data, c(N, G, dg, Do, Ho, SDI, BAL, ALT))




# set seed for reproducibility in stochastic processes
set.seed(42)



# 2: Correlation Analysis ====

# calculate pearson correlation matrix
cor_matrix <- cor(vars, method = "pearson")

# print matrix to console
print("Pearson Correlation Matrix:")
print(round(cor_matrix, 3))



# 3: Visualization ====

# create scannable pairs plot with correlation coefficients
corr_plot <- ggpairs(
  data = vars,
  lower = list(continuous = wrap("points", alpha = 0.6, size = 1.5)),
  upper = list(continuous = wrap("cor", size = 4.5, color = "black")),
  diag = list(continuous = wrap("densityDiag", fill = "grey90"))
) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

corr_plot
# results:
# - N and SDI > 0.85 -> N discarted
# - dg and DO > 0.85 -> Do discarted

ggsave(
  filename = "variables_correlation_plot.png",
  plot = corr_plot,
  path = "/home/alicia/Uni/TFG/Modelo_extendido",
  width = 8,
  height = 8,
  dpi = 300)
