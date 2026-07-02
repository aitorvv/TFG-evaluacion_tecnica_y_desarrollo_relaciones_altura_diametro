#!/usr/bin/Rscript

# Support functions ----
#
# Aitor Vázquez Veloso
# 2026-06-01
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# helper function to calculate R-squared
calculate_r_squared <- function(observed, predicted, residuals) {
  ss_total <- sum((observed - mean(predicted))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  return(r_squared)
}

# helper function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# helper function to calculate MAE
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}

# helper function to calculate BIAS
calculate_bias <- function(observed, predicted) {
  mean(predicted - observed)
}
