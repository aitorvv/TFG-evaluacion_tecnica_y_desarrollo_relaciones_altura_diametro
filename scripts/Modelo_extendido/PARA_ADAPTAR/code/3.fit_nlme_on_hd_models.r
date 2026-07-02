#!/usr/bin/Rscript

# Code to fit nlme by using the best height-diameter models ----
#
# Aitor Vázquez Veloso
# 2026-06-02
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# set working directory
setwd("/media/aitor/7FB15A9C1FA8F1DA/trabajo/publicaciones/202x_temesgen")



# Load data and functions ====

# install and load necessary libraries
library(nlme)
library(tidyverse)
library(broom.mixed)  # useful for extracting residuals, fitted values, etc.

# load functions
source("code/0.support_functions.r")
source("code/0.hd_equations_collection.r")
hd_models_collection <- get_models_list()

# load data
tree_data <- read.csv("data/swodata.csv")
tree_data$species <- as.factor(tree_data$spp)
tree_data$h <- tree_data$ht  # consistency with model collection

# top models data set
top_hd_models <- read.csv("output/2.top_hd_models.csv")

# combine data set with coefficients
base_models_coefs <- read.csv("output/1.fit_base_hd_models_coefs.csv")
top_hd_models <- top_hd_models %>%
  dplyr::inner_join(base_models_coefs, by = c("species", "model"))

# remove redundant data sets
rm(base_models_coefs)



# Set up fixed and random effects combinations ====

# fixed effects
# fe_to_eval <- c("1", "ccfl", "bal", "ccfl + bal")
fe_to_eval <- c("1", "ccfl + bal")

# random effects
# NOTE: can be consider a variable belonging to other variable like "stand / plot"
# re_to_eval <- c("1", "standid", "plotid", "standid / plotid")
re_to_eval <- c("1", "standid")
re_to_eval_binary <- c("0", "1")  # 0 means no random effect (using gnls()), 1 means include random effect (using nlme())



# Fit the nlme models ====

# initialize empty list to store results
dir.create("output/3.nlme_hd_models", recursive = TRUE, showWarnings = FALSE)  # create dir to save models
results_stats <- data.frame()  # to store model comparison stats (aic, bic, etc.)

cat("Starting nlme model fitting process across species...\n")



## Species selection ====

for (sp in unique(tree_data$species)) {
  
  # filter data for the current species group
  species_data <- tree_data %>% dplyr::filter(species == sp)
  species_top_models <- top_hd_models %>% dplyr::filter(species == sp)

  cat("Processing species: ", as.character(sp), "...\n", sep = "")
  
  
  
  ## Model selection ====
  
  for (model_name in unique(species_top_models$model)) {
    
    
    
    ### General model configuration ----

    # get model coefficients
    start_values <- species_top_models %>% dplyr::filter(model == model_name)
    start_values_list <- as.list(setNames(start_values$estimate, start_values$term))
    model_coefs <- start_values_list[order(names(start_values_list))]

    # get model details
    model_function <- hd_models_collection[[model_name]]$func
    coefs_labels <- names(model_coefs)
    formula_str <- paste0("h ~ model_function(", paste(coefs_labels, collapse = ", "), ", dbh)")
    model_formula <- as.formula(formula_str)

    
        
    ### Fixed effects alternatives ----

    # generate all possible permutations for the number of parameters the model has
    fe_combis_to_eval <- expand.grid(replicate(length(coefs_labels), fe_to_eval, simplify = FALSE), 
                                     stringsAsFactors = FALSE)

    # create the list of formulas
    fe_combis <- list()
    for (r in 1:nrow(fe_combis_to_eval)) {
      form_list <- list()
      for (c in 1:length(coefs_labels)) {
        form_list[[c]] <- as.formula(paste(coefs_labels[c], "~", fe_combis_to_eval[r, c]))
      }
      fe_combis <- c(fe_combis, list(form_list))
    }
    # NOTE: this approach automatically adapts to models with any number of parameters

    
    
    ### Random effects alternatives ----

    # get all the possible combinations of terms ~ random effects
    re_combis_to_eval <- expand.grid(replicate(length(coefs_labels), re_to_eval_binary, simplify = FALSE), 
                                     stringsAsFactors = FALSE)

    # create the list of formulas
    re_combis <- list()
    for (r in 1:nrow(re_combis_to_eval)) {
      # if all parameters have "0", we define a special case "none" to trigger gnls()
      if (all(as.character(re_combis_to_eval[r, ]) == "0")) {
        re_combis <- c(re_combis, list("none"))
        next
      }

      # select the parameters that have a '1' in this combination
      active_params <- coefs_labels[as.numeric(re_combis_to_eval[r, ]) == 1]

      # create the left-hand side of the formula: e.g., "a + b"
      lhs <- paste(active_params, collapse = " + ")

      # loop through the available grouped random effects
      # NOTE: if you have multiple 're_to_eval' that are not just binary, you would expand this inner loop
      for (random_group in re_to_eval[re_to_eval != "1"]) {
        form_str <- paste(lhs, "~ 1 |", random_group)
        re_combis <- c(re_combis, list(as.formula(form_str)))
      }
    }
    
    

    ### Model fit ----

    cat("Fitting models for species: ", as.character(sp), " and model: ", model_name, "...\n", sep = "")

    # loop over fixed effects combinations
    for (i in 1:length(fe_combis)) {
      fe_combi <- fe_combis[[i]]

      # build the start vector dynamically
      start_vec <- c()
      for (k in seq_along(fe_combi)) {
        # extract parameter name from the left-hand side of the formula (e.g., a0 from a0 ~ 1 + ccfl)
        p_name <- as.character(fe_combi[[k]][[2]])

        # the base starting value from the non-linear squares coefficient list
        base_val <- model_coefs[[p_name]]

        # extract the rhs of the formula (e.g., ~ 1 + ccfl)
        rhs_formula <- formula(delete.response(terms(fe_combi[[k]])))

        # dynamically build model matrix to support continuous or categorical covariates (factors)
        # model.matrix handles factors automatically, outputting k-1 columns for k levels
        m_frame <- model.frame(rhs_formula, data = species_data, na.action = na.pass)
        m_matrix <- model.matrix(rhs_formula, data = m_frame)
        n_extra_params <- ncol(m_matrix) - 1

        # append the base value, and fill with 0s for the extra covariates
        start_vec <- c(start_vec, base_val, rep(0, n_extra_params))
      }

      # (random effects don't require start values to be passed to nlme)

      # loop over random effects combinations
      for (j in 1:length(re_combis)) {
        re_combi <- re_combis[[j]]

        # note: the start values vector must have the exact correct length corresponding to the fixed effects parameters
        # in a real scenario, you usually calculate a vector of initials based on evaluating 'fe_combi'
        # (e.g. padding zeroes for added covariables). for now, it assumes model_coefs has everything needed.

        cat("  - fitting fe combination ", i, " and re combination ", j, "...\n", sep = "")
        fit_result <- tryCatch(
          {
            # if the random effect combination is "none", fit gnls instead of nlme
            if (inherits(re_combi, "character") && re_combi == "none") {
              fit <- nlme::gnls(
                model = model_formula,
                data = species_data,
                params = fe_combi,
                start = start_vec
              )
            } else {
              fit <- nlme::nlme(
                model = model_formula,
                data = species_data,
                fixed = fe_combi,
                random = re_combi,
                start = start_vec
              )
            }
          },
          error = function(e) {
            cat("WARNING: Fitting failed for model ", model_name, " fe: ", i, " re: ", j, " - ", e$message, "\n", 
                sep = "")
            return(NULL) # return NULL if there's an error
          }
        )

        # check if the model fit was successful
        if (!is.null(fit_result)) {
          
          # predict the height based on the model
          species_data$predicted_h <- predict(fit)

          # model stats
          model_stats <- tibble::tibble(
            species = as.character(sp),
            model_name = as.character(model_name),
            fe_combi = paste(sapply(fe_combi, deparse), collapse = "; "),
            n_fe_combi = as.integer(i),
            re_combi = paste(deparse(re_combi), collapse = ""),
            n_re_combi = as.integer(j),
            start = paste(unlist(model_coefs), collapse = "; "),
            r_squared = calculate_r_squared(species_data$h, species_data$predicted_h, residuals(fit)),
            rmse = calculate_rmse(species_data$h, species_data$predicted_h),
            mae = calculate_mae(species_data$h, species_data$predicted_h),
            bias = calculate_bias(species_data$h, species_data$predicted_h),
            bic = as.numeric(BIC(fit)),
            aic = as.numeric(AIC(fit)),
            loglik = as.numeric(logLik(fit)),
            error = FALSE  # mark as error if fitting failed
          )

          # save model to disk to prevent out of memory issues
          saveRDS(fit, file = paste0("output/3.nlme_hd_models/", sp, "_", model_name, "_fe_", i, "_re_", j, ".rds"))
          
        } else {
          # model stats
          model_stats <- tibble::tibble(
            species = as.character(sp),
            model_name = as.character(model_name),
            fe_combi = paste(sapply(fe_combi, deparse), collapse = "; "),
            n_fe_combi = as.integer(i),
            re_combi = paste(deparse(re_combi), collapse = ""),
            n_re_combi = as.integer(j),
            start = paste(unlist(model_coefs), collapse = "; "),
            r_squared = NA_real_,
            rmse = NA_real_,
            mae = NA_real_,
            bias = NA_real_,
            bic = NA_real_,
            aic = NA_real_,
            loglik = NA_real_,
            error = TRUE  # mark as error if fitting failed
          )

          # no model to save since it failed
        }

        results_stats <- dplyr::bind_rows(results_stats, model_stats)
      }
    }
  }
}

# Save model stats ====

results_stats <- results_stats %>% arrange(species, aic)
write.csv(results_stats, 'output/3.nlme_models_stats.csv', row.names = FALSE)
cat("Script execution finished successfully!\n")
