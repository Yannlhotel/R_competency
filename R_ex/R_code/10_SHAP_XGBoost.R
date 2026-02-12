# ============================================================================== #
# Title: XGBoost Model and SHAP explanations
# Purpose: Modeling and Understanding "User Retention" using XGBoost + Shapviz
# Task: Fill in the missing code segments marked with TODO or _______
# ============================================================================== #
rm(list = ls(all.names = TRUE))

library(tidyverse)
library(xgboost)  # Standard CRAN package
library(shapviz)
library(patchwork)
library(pROC)     # For AUC calculation

set.seed(0)

# 0. Global Theme Settings ----
theme_set(theme_light())

# Define consistent colors
col_increase <- "#E60032" # Red
col_decrease <- "#005b7f" # Blue 

# ============================================================================== #
# 1. LOAD DATA ----
# ============================================================================== #

# Load the dataset
path_to_file <- "C:/Users/yannl/Documents/BHT/ML2/R_ex/data/user_retention_data.csv"
if(!file.exists(path_to_file)) {
  stop("Error: 'user_retention_data.csv' not found. Please download the data first.")
}

print("--- Loading Data ---")
df_full <- read_csv(path_to_file)

# Separate Target (y) and Features (X)
y <- df_full$`Did renew`
X <- df_full %>% select(-`Did renew`)

# Quick inspection
print(paste("Observations:", nrow(X)))
print(paste("Features:", ncol(X)))
head(X)

# ============================================================================== #
# 2. XGBOOST TRAINING ----
# ============================================================================== #

print("--- Training XGBoost Model ---")

# TODO: XGBoost requires a specific matrix format called `xgb.DMatrix`.
# We convert X to a matrix.
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Define parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.05,
  max_depth = 2,
  colsample_bytree = 0.5
)

# TODO: Train the XGBoost model. Use nrounds = 500.
model_xgb <- xgb.train(params = params, data = dtrain, nrounds = 500, verbose = 0)

# ============================================================================== #
# 2.1 MODEL EVALUATION (In-Sample) ----
# ============================================================================== #

print("--- Evaluating Model Performance ---")

# TODO: Generate predictions for the calculation of AUC.
preds <- predict(model_xgb, dtrain)

# TODO: Calculate AUC using the pROC package.
auc_score <- pROC::auc(y, preds)
print(paste("In-sample AUC:", round(auc_score, 4)))

# ============================================================================== #
# 3. MANUAL SHAP ANALYSIS (EDUCATIONAL) ----
# ============================================================================== #

print("--- Manual SHAP Calculations ---")

# 1. Extract raw SHAP values from XGBoost
# TODO: Use the `predict` function with `predcontrib = TRUE`.
raw_shap <- predict(model_xgb, as.matrix(X), predcontrib = TRUE)

# 2. Separate Contributions matrix and Bias vector
# Bias (Base Value) is located in the LAST column.

# TODO: Subset the matrix to get only feature contributions (all columns except last)
shap_contribs <- raw_shap[, -ncol(raw_shap)] 

# TODO: Extract the Bias (Take the first value of the last column)
shap_bias <- raw_shap[1, ncol(raw_shap)] 

# Assign names to the matrix columns
colnames(shap_contribs) <- colnames(X)

# --- A. Manual Feature Importance Plot ---
# Definition: Mean of absolute SHAP values
imp_df <- data.frame(
  Feature = colnames(shap_contribs),
  Importance = colMeans(abs(shap_contribs)) 
)

p_manual_imp <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = col_decrease) +
  coord_flip() +
  labs(title = "Manual Feature Importance", x = "", y = "mean(|SHAP value|)")

print(p_manual_imp)

# --- B. Manual Dependence Plot (Ad Spend) ---
feature_name <- "Ad spend"
dep_df <- data.frame(
  Feature_Value = X[[feature_name]],
  # TODO: Extract the SHAP values for the specific feature
  SHAP_Value = shap_contribs[, feature_name]
)

p_manual_dep <- ggplot(dep_df, aes(x = Feature_Value, y = SHAP_Value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, color = col_increase) +
  labs(title = paste0("Manual Dependence: ", feature_name), 
       subtitle = "Shows spurious correlation",
       x = paste0(feature_name, " (Feature)"), y = "SHAP Contribution")

print(p_manual_dep)

# --- C. Manual Microscopic Explanation (Obs #1) ---
obs_idx <- 1

# TODO: Subset the `shap_contribs` to obtain values for row obs_idx
micro_df <- data.frame(
  Feature = colnames(shap_contribs),
  Contribution = shap_contribs[obs_idx, ]
)

# TODO: Create a "Type" column (Increase if > 0, Decrease otherwise)
micro_df$Type <- ifelse(micro_df$Contribution > 0, "Increase", "Decrease")

p_manual_micro <- ggplot(micro_df, aes(x = reorder(Feature, abs(Contribution)), 
                                       y = Contribution, fill = Type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Increase" = col_increase, 
                               "Decrease" = col_decrease)) +
  labs(title = paste0("Manual Breakdown: Obs #", obs_idx),
       subtitle = paste0("Bias: ", round(shap_bias, 2), 
                         " + Sum(Contribs): ", round(sum(micro_df$Contribution), 2),
                         " = Prediction: ", round(shap_bias + sum(micro_df$Contribution), 2)))

print(p_manual_micro)

# ============================================================================== #
# 4. CREATE SHAPVIZ OBJECT ----
# ============================================================================== #

print("--- Creating Shapviz Object ---")

shap_obj_xgb <- shapviz(shap_contribs, X = X, baseline = shap_bias)

# ============================================================================== #
# 5. SHAPVIZ MICROSCOPIC PLOTS (Waterfall & Force) ----
# ============================================================================== #

print("--- Generating Waterfall and Force Plots ---")

sv_cols <- c(col_decrease, col_increase) # Order: Negative, Positive

obs_indices <- c(1, 10)

# --- A. Waterfall Plot ---
# TODO: Generate a waterfall plot
p_waterfall <- sv_waterfall(shap_obj_xgb, row_id = obs_indices[1]) +
  ggtitle(paste0("Waterfall Plot: Obs #", obs_indices[1])) +
  scale_fill_manual(values = sv_cols)

print(p_waterfall)

# --- B. Force Plot ---
# TODO: Generate a force plot
p_force <- sv_force(shap_obj_xgb, row_id = obs_indices[1]) +
  ggtitle(paste0("Force Plot: Obs #", obs_indices[1])) +
  scale_fill_manual(values = sv_cols)

print(p_force)

# ============================================================================== #
# 6. SHAPVIZ GLOBAL PLOTS ----
# ============================================================================== #

print("--- Generating Global Plots ---")

# A. Importance Plot (Beeswarm)
# TODO: Create a Beeswarm plot using `sv_importance` and kind = "bee"
p_imp_bee <- sv_importance(shap_obj_xgb, kind = "bee") + 
  ggtitle("XGBoost: Feature Importance (Beeswarm)") +
  scale_color_gradient(low = col_decrease, high = col_increase)

print(p_imp_bee)

# Importance Plot (Bar)
p_imp_bar <- sv_importance(shap_obj_xgb, kind = "bar", fill = col_decrease) + 
  ggtitle("XGBoost: Feature Importance (Bar)")

print(p_imp_bar)

# B. Dependence Plots (Grid)
p_dep <- sv_dependence(shap_obj_xgb, 
                       v = colnames(X), 
                       color_var = NULL) +
  scale_color_gradient(low = col_decrease, high = col_increase) +
  plot_layout(ncol = 3)

print(p_dep)

p_dep_ylim_fixed <- sv_dependence(shap_obj_xgb, 
                                  v = colnames(X), 
                                  color_var = NULL, 
                                  ylim = range(shap_contribs)) +
  scale_color_gradient(low = col_decrease, high = col_increase) +
  plot_layout(ncol = 3)
print(p_dep_ylim_fixed)

# TODO: Discuss why `ylim = range(shap_contribs)` is useful.
# Answer: Using a fixed ylim across all dependence plots allows for a fair 
# comparison of the "strength" of each feature's effect. It visually communicates 
# which features have a large impact on the prediction range and which are 
# relatively flat/unimportant.