# ==============================================================================
# INTERPRETABLE MACHINE LEARNING (IML) - INSTRUCTIONAL DEMO (DALEX)
# ==============================================================================
# This script demonstrates how to use the 'DALEX' ecosystem to analyze 
# machine learning models.
# Methods covered:
# 1. Permutation Feature Importance (PFI) -> model_parts()
# 2. Partial Dependence Plots (PDP)       -> model_profile(type = "partial")
# 3. Accumulated Local Effects (ALE)      -> model_profile(type = "accumulated")
# 4. SHAP values                          -> predict_parts(type = "shap")
# ==============================================================================
install.packages('DALEX')
# 1. SETUP AND LIBRARIES -------------------------------------------------------
library(DALEX)        # Core interpretability library
library(DALEXtra)     # Extension for wrappers and surrogates
library(localModel)   # Native LIME-like implementation for DALEX
library(randomForest) # Random Forest
library(xgboost)      # Gradient Boosting
library(e1071)        # SVM
library(splines)      # For Linear Model with Splines
library(mgcv)         # For Generalized Additive Models (GAM)
library(ggplot2)      # Visualization
library(patchwork)    # For arranging plots
library(dplyr)        # Data manipulation
library(gridExtra)    # Grid layouts

set.seed(42) # Ensure reproducibility

# 2. DATA SIMULATION (Ground Truth) --------------------------------------------
# We simulate data where we KNOW the true relationship.

x_range <- c(-1, 1)
n <- 500

X <- data.frame(
  x1 = runif(n, x_range[1], x_range[2]),
  x2 = runif(n, x_range[1], x_range[2]),
  x3 = runif(n, x_range[1], x_range[2]),
  x4 = runif(n, x_range[1], x_range[2]) # Noise feature
)

# Define the true underlying function
true_fn <- function(x1, x2, x3) {
  return(2 * x1 + sin(x2 * 6) + 3 * x3^2 - 2)
}

y <- true_fn(X$x1, X$x2, X$x3) + rnorm(n, sd = 0.5)
data <- cbind(X, y = y)

# 3. MODEL TRAINING ------------------------------------------------------------
cat("Training models...\n")
cat("NOTE: In this instructional demo, we use default hyperparameters for simplicity.\n")

# A. Linear Model (Baseline)
mod_lm <- lm(y ~ ., data = data)

# B. Linear Model with Splines (Flexible Linear)
# We use natural splines (ns)  to capture non-linearity
mod_spline <- lm(y ~ ns(x1, 4) + ns(x2, df=4) + ns(x3, df=4) + ns(x4, 4), data = data)

# C. Generalized Additive Model (GAM)
# We use smoothing splines s() for all predictors to automatically learn non-linearity
mod_gam <- gam(y ~ s(x1) + s(x2) + s(x3) + s(x4), data = data)

# D. Support Vector Machine (SVM)
mod_svm <- svm(y ~ ., data = data)

# E. Random Forest
mod_rf <- randomForest(y ~ ., data = data, ntree = 500)

# F. XGBoost
# XGBoost requires matrix format
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)
mod_xgb <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)


# 4. CREATE DALEX EXPLAINERS ---------------------------------------------------
cat("Creating DALEX Explainers...\n")

# Custom predict function needed for GAM 
# mgcv::gam returns a 1D array, which confuses `localModel` in section 8. 
# We force it to be a plain numeric vector.
custom_predict_gam <- function(model, newdata) {
  as.numeric(predict(model, newdata))
}

# Custom predict function needed for XGBoost to handle matrix conversion
custom_predict_xgb <- function(model, newdata) {
  predict(model, as.matrix(newdata))
}

# Create Explainers
# Since there exist multiple `explain()` functions in different loaded
# packages, we use `DALEX::explain()` to specify the package from which 
# `explain()` should be used.
exp_lm     <- DALEX::explain(mod_lm, data = X, y = y, label = "Linear Model", verbose = FALSE)
exp_spline <- DALEX::explain(mod_spline, data = X, y = y, label = "LM Splines", verbose = FALSE)
exp_gam    <- DALEX::explain(mod_gam, data = X, y = y, predict_function = custom_predict_gam,
                             label = "GAM (mgcv)", verbose = FALSE)
exp_svm    <- DALEX::explain(mod_svm, data = X, y = y, label = "SVM", verbose = FALSE)
exp_rf     <- DALEX::explain(mod_rf, data = X, y = y, label = "Random Forest", verbose = FALSE)
exp_xgb    <- DALEX::explain(mod_xgb, data = X, y = y, predict_function = custom_predict_xgb, 
                      label = "XGBoost", verbose = FALSE)


# Group explainers in a list for iteration
explainers <- list(
  LM     = exp_lm,
  Spline = exp_spline,
  GAM    = exp_gam,
  SVM    = exp_svm,
  RF     = exp_rf,
  XGB    = exp_xgb
)

# 5. PREDICTIVE PERFORMANCE ----------------------------------------------------
cat("Calculating Predictive Performance on Test Data...\n")

# To properly evaluate performance, we simulate a FRESH test set.
X_test <- data.frame(
  x1 = runif(n, x_range[1], x_range[2]),
  x2 = runif(n, x_range[1], x_range[2]),
  x3 = runif(n, x_range[1], x_range[2]),
  x4 = runif(n, x_range[1], x_range[2])
)
y_test <- true_fn(X_test$x1, X_test$x2, X_test$x3) + rnorm(n, sd = 0.5)

# Calculate performance using DALEX::model_performance
perf_list <- lapply(explainers, function(exp) {
  # Temporarily update the explainer with the test data
  exp_test <- update_data(exp, data = X_test, y = y_test, verbose = FALSE)
  model_performance(exp_test)
})

# Plot performance
# DALEX has a built-in plot, but we can customize or use the default
# Combining all performance objects
p_perf <- plot(perf_list$LM, perf_list$Spline, perf_list$GAM, 
               perf_list$SVM, perf_list$RF, perf_list$XGB) +
  ggtitle("Predictive Performance (RMSE/MAE etc)") +
  theme_light()
print(p_perf)

# Performance measures obtained based on the residuals
lapply(perf_list, function(x) x$measures) |> 
  bind_rows(.id = "model") |> 
  arrange(mad)

# 6. PERMUTATION FEATURE IMPORTANCE (PFI) --------------------------------------
cat("Calculating Permutation Feature Importance...\n")

plot_pfi <- list()

# For regression the standard loss function applied is RMSE.
# We can change this providing a custom loss function:
loss_mae <- function(observed, predicted){
  mean(abs(observed - predicted))
}

for (name in names(explainers)) {
  # Calculate PFI using model_parts()
  pfi <- model_parts(explainers[[name]], loss_function = loss_mae, type = "difference")
  
  p <- plot(pfi) + 
    ggtitle(paste("PFI:", name)) + 
    theme_light() +
    theme(legend.position = "none")
  
  plot_pfi[[name]] <- p
}

# Display PFI comparison
grid.arrange(grobs = plot_pfi, ncol = 3, top = "Global Feature Importance Comparison (Loss: MAE)")

# 7. FEATURE EFFECTS: PDP vs ALE -----------------------------------------------
cat("Calculating Feature Effects (PDP & ALE)...\n")

features_to_plot <- c("x1", "x2", "x3", "x4")

for(feature_to_plot in features_to_plot){

  cat(paste("Generating plots for feature:", feature_to_plot, "\n"))
  
  # --- DEFINE TRUE RELATIONSHIP FOR PLOTTING ---
  # Generate grid for ground truth line
  # Check "Supplement Section" on Exercise Sheet for detailed explanation
  x_grid <- seq(x_range[1], x_range[2], length.out = 200)
  
  if (feature_to_plot == "x1") {
    true_pdp_y <- 2 * x_grid - 1
    true_ale_y <- 2 * x_grid 
  } else if (feature_to_plot == "x2") {
    true_pdp_y <- sin(x_grid * 6) - 1
    true_ale_y <- sin(x_grid * 6)
  } else if (feature_to_plot == "x3") {
    true_pdp_y <- 3 * x_grid^2 - 2
    true_ale_y <- 3 * x_grid^2 - 1 
  } else if (feature_to_plot == "x4") {
    true_pdp_y <- x_grid * 0 - 1
    true_ale_y <- x_grid * 0
  }
  
  # Create DataFrames for plotting
  true_pdp_df <- data.frame(feature = x_grid, y = true_pdp_y)
  colnames(true_pdp_df)[1] <- feature_to_plot 
  
  true_ale_df <- data.frame(feature = x_grid, y = true_ale_y)
  colnames(true_ale_df)[1] <- feature_to_plot
  
  pdp_plots <- list()
  ale_plots <- list()
  
  for (name in names(explainers)) {
    
    # --- PDP ---
    pdp <- model_profile(explainers[[name]], variables = feature_to_plot, type = "partial")
    
    pdp_p <- plot(pdp) + 
      geom_line(data = true_pdp_df, aes(x = .data[[feature_to_plot]], y = y), 
                color = "red", linetype = "dotted", linewidth = 1, inherit.aes = FALSE) +
      ggtitle(paste0("PDP ", feature_to_plot, ": ", name)) + 
      ylim(-2.5, 2.5) + 
      theme_light() + theme(legend.position = "none")
    
    pdp_plots[[name]] <- pdp_p
    
    # --- ALE ---
    ale <- model_profile(explainers[[name]], variables = feature_to_plot, type = "accumulated")
    
    # Manual correction so that ALE plots are centered around 0 which should be the default 
    ale$agr_profiles$`_yhat_` <- ale$agr_profiles$`_yhat_` - mean(ale$agr_profiles$`_yhat_`)
    
    ale_p <- plot(ale) + 
      geom_line(data = true_ale_df, aes(x = .data[[feature_to_plot]], y = y), 
                color = "red", linetype = "dotted", linewidth = 1, inherit.aes = FALSE) +
      ggtitle(paste("ALE ", feature_to_plot, ": ", name)) + 
      ylim(-2.5, 2.5) + 
      theme_light() + theme(legend.position = "none")
    
    ale_plots[[name]] <- ale_p
  }
  
  # Visualize PDP Comparison
  grid.arrange(grobs = pdp_plots, ncol = 3, 
               top = paste0("Partial Dependence Plots (PDP) - Feature ", feature_to_plot))
  
  # Visualize ALE Comparison
  grid.arrange(grobs = ale_plots, ncol = 3, 
               top = paste0("Accumulated Local Effects (ALE) - Feature ", feature_to_plot))
  
}

# 8. SHAP VALUES (Local Interpretation) ----------------------------------------
cat("Calculating SHAP values (Approximate Shapley values)...\n")
# SHAP assigns a contribution to each feature based on game theory.
# The sum of SHAP values + Intercept = Prediction.

obs_id <- 100 
new_obs <- X[obs_id, ]
print(new_obs)

shap_plots <- list()
for (name in names(explainers)) {
  
  # Calculate SHAP
  # type = "shap" calculates the average contribution over B random paths.
  # B = 25 is a common default for a quick approximation. 
  # Higher B (e.g., 50 or 100) reduces variance but takes longer.
  shap <- predict_parts(explainer = explainers[[name]], 
                        new_observation = new_obs, 
                        type = "shap", 
                        B = 25)
  
  # Plot SHAP
  # The plot shows the mean contribution (bars) and the distribution 
  # of contributions across the B permutations (boxplots/whiskers),
  # indicating the stability of the explanation.
  shap_plots[[name]] <- plot(shap) + 
    ggtitle(paste("SHAP:", name)) + 
    theme_light() +
    theme(legend.position = "none") 
}

# Display SHAP comparison
grid.arrange(grobs = shap_plots, ncol = 3, 
             top = paste0("SHAP Explanations for Observation #", obs_id))

# 9. PREDICTING SHAP FROM ALE PLOTS --------------------------------------------

# choose model for the analysis
model <- "GAM"

# --- ALE plots for features ---
cat("Generating Global ALE Panel for ", model, " Model...\n", sep = "")

# Calculate ALE profiles for all 4 features at once
ale_effects <- model_profile(explainers[[model]], 
                             variables = c("x1", "x2", "x3", "x4"), 
                             type = "accumulated")
# manual correction to obtain centered ALE plots
class_to_restore <- class(ale_effects$agr_profiles)
ale_effects$agr_profiles <- ale_effects$agr_profiles |> 
  group_by(`_vname_`) |> 
  mutate(`_yhat_` = `_yhat_` - mean(`_yhat_`))
class(ale_effects$agr_profiles) <- class_to_restore

# Plot them as a faceted panel
plot(ale_effects) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  ggtitle(paste0("Global ALE Plots: ", model, " Model (Reference for SHAP)")) +
  theme_light()

# -------------- IMPORTTANT: NOW READ EXERCISE SHEET INSTRUCTIONS

# --- Case A: Testing x1 ---

# Find observation where x1 is closest to your target value
target_val_x1 <- TARGET_VALUE
obs_id <- which.min(abs(X$x1 - target_val_x1))
new_obs <- X[obs_id, ]
print(new_obs)

cat(paste0("Testing Observation #", obs_id, " where x1 = ", round(X$x1[obs_id], 2), "\n"))

# Run SHAP for this observation
shap <- predict_parts(explainer = explainers[[model]], # or RF
                      new_observation = new_obs, 
                      type = "shap", B = 25)
plot(shap) + ggtitle(paste("Case A: x1 target =", target_val_x1))


# --- Case B: Testing x2 ---
target_val_x2 <- TARGET_VALUE 
obs_id <- which.min(abs(X$x2 - target_val_x2))
new_obs <- X[obs_id, ]
print(new_obs)

cat(paste0("Testing Observation #", obs_id, " where x2 = ", round(X$x2[obs_id], 2), "\n"))

# Run SHAP for this observation
shap <- predict_parts(explainer = explainers[[model]], # or RF
                      new_observation = X[obs_id, ], 
                      type = "shap", B = 25)
plot(shap) + ggtitle(paste("Case B: x2 target =", target_val_x2))


# --- Case C: Testing x3 ---
target_val_x3 <- TARGET_VALUE
obs_id <- which.min(abs(X$x3 - target_val_x3))
new_obs <- X[obs_id, ]
print(new_obs)

cat(paste0("Testing Observation #", obs_id, " where x3 = ", round(X$x3[obs_id], 2), "\n"))

# Run SHAP for this observation
shap <- predict_parts(explainer = explainers[[model]], # or RF
                      new_observation = X[obs_id, ], 
                      type = "shap", B = 25)
plot(shap) + ggtitle(paste("Case C: x3 target =", target_val_x3))
