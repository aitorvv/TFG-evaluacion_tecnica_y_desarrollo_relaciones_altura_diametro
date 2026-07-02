#!/usr/bin/Rscript

# Select top 5 models from candidates and filter model results ----
#
# Aitor Vázquez Veloso
# 2024-10-01 adapted on 2026-06-02
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# working directory
setwd('')

# libraries
library(tidyverse)  

# load data
df <- read.csv('output/1.fit_base_hd_models_raw_results.csv')



# Top 5 models selection + Chapman Richards ----

df_top <- df %>%
  group_by(species) %>%
  arrange(aic) %>%
  slice_head(n = 5)
  
cr <- c('chapman_richards_1959', 'chapman_richards_model')
df_cr <- df[df$model %in% cr, ]

df_final <- rbind(df_top, df_cr)



# Save results ----
write.csv(df_final, 'output/2.top_hd_models.csv', row.names = FALSE)
