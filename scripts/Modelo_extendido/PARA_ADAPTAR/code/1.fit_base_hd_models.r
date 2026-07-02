#!/usr/bin/Rscript

# Fit the better hd model for each data set provided ----
#
# Aitor Vázquez Veloso
# 2024-09-26 adapted on 2026-06-01
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# working directory
setwd('')

# libraries
library(tidyverse)  # data manipulation
library(broom)  # tidying model outputs
library(minpack.lm)  # for nlsLM



# Load data and functions ====

# load functions
source('code/0.hd_equations_collection.r')
source('code/0.support_functions.r')

# load data
df <- read.csv('data/swodata.csv')
str(df)

# same height variable name to model collection
df$h <- df$ht

# species groups
df$spp <- as.factor(df$spp)
sp_groups <- unique(df$spp)



# Model selection and start coefficients management ====

# list of models to fit based just on dbh as predictor
models <- get_models_list()



# Fit the models ====

# initialize empty list to store results
results <- list()

# for loop for species group
for(sp in sp_groups){

  # filter data for the current species group
  df_sp <- df %>% filter(spp == sp)

  # for loop to fit each model
  for(model_name in names(models)) {
    
    # extract model information and set default values
    model_function <- models[[model_name]]$func
    start_params <- models[[model_name]]$start
    fit_success <- FALSE
    
    # dynamically create the formula for nls adapted to each model
    param_names <- names(start_params)
    formula_str <- paste0("h ~ model_function(", paste(param_names, collapse = ", "), ", dbh)")
    model_formula <- as.formula(formula_str)
    
    # try fitting the model
    fit_result <- tryCatch({
      
      fit <- nlsLM(model_formula, data = df_sp, start = start_params)
      
      # predicted values
      predicted <- predict(fit, newdata = df_sp)
      
      # calculate R2, RMSE, MAE, BIAS
      r_squared <- calculate_r_squared(df_sp$h, predicted)
      rmse <- calculate_rmse(df_sp$h, predicted)
      mae <- calculate_mae(df_sp$h, predicted)
      bias <- calculate_bias(df_sp$h, predicted)
      
      # calculate BIC, AIC, logLik
      bic <- BIC(fit)
      aic <- AIC(fit)
      loglik <- logLik(fit)
      
      # if the fit is successful, mark success and return the model info
      fit_success <- TRUE        
      print(paste('Model ', model_name, " fitted successfully for species ", sp, sep = ''))
      list(species = sp,
           model = model_name, 
           bic = bic,            
           r_squared = r_squared,
           rmse = rmse,          
           mae = mae,
           bias = bias,
           aic = aic, 
           loglik = loglik,
           coefficients = tidy(fit))
      
    }, error = function(e) {
      
      # in case of error, print it and return NULL
      message(paste("Error in model ", model_name, " for species ", sp, ": ", e$message, sep = ''))
      
    })
    
    # if successful, store the result; if not, store NA values for the metrics
    results_key <- paste(sp, model_name, sep = "_")
    if (!is.null(fit_result)) {
      results[[results_key]] <- fit_result
    } else {
      results[[results_key]] <- list(species = sp, model = model_name, bic = NA, r_squared = NA, rmse = NA, 
                                    mae = NA, bias = NA, aic = NA, loglik = NA, coefficients = NA)
    }

  }
  
}

# save the results in an .rdata file for later use
save(results, file = 'output/1.fit_base_hd_models_raw_results.rdata')
# load('output/fit_base_hd_models_raw_results.rdata')



# Ranking and export results ====

# convert the results into a tidy data frame for easy comparison
results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(species = x$species, model = x$model, bic = x$bic, r_squared = x$r_squared, rmse = x$rmse,
             mae = x$mae, bias = x$bias, aic = x$aic, loglik = x$loglik)
}))
results_df$results_key <- paste(results_df$species, results_df$model, sep = "_")

# show the results ordered by species and AIC (lower AIC is better)
results_df <- results_df %>%
  arrange(species, aic)

# export ordered results in a .csv file
write.csv(results_df, "output/1.fit_base_hd_models_raw_results.csv", row.names = FALSE)

# export the coefficients of the models that were able to fit
results_df <- results_df[!is.na(results_df$aic), ]
coefs <- tibble(species = character(), model = character(), term = character(), estimate = numeric(), std.error = numeric(),
                statistic = numeric(), p.value = numeric())

for(fitted_model in results_df$results_key){

  # filter model information
  model <- results[[fitted_model]]
  model <- bind_cols(species = model$species, model = model$model, model$coefficients)

  # append to the coefficients data frame
  coefs <- rbind(coefs, model)
}

# export coefficients
write.csv(coefs, "output/1.fit_base_hd_models_coefs.csv", row.names = FALSE)

