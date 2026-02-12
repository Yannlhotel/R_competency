# ============================================================================== #
# Title: Catboost Model and SHAP explanations 
# Purpose: Modeling and Understanding "User Retention" using CatBoost + Shapviz
# Task: Fill in the missing code segments marked with TODO or _______
# ============================================================================== #
rm(list = ls(all.names = TRUE))

library(tidyverse)
library(catboost) # Requires manual installation
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
# Ensure 'user_retention_data.csv' is in your Data directory
path_to_file <- "Data/user_retention_data.csv"
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
# 2. CATBOOST TRAINING ----
# ============================================================================== #

print("--- Training CatBoost Model ---")

# CatBoost requires a specific 'pool' object
train_pool <- catboost.load_pool(data = X, label = y)

# Define parameters
params <- list(
  loss_function = 'Logloss',
  iterations = 500,
  depth = 2,
  learning_rate = 0.05,
  rsm = 0.5,
  verbose = 0
)

# Train the model
model_cat <- catboost.train(train_pool, params = params)

# NOTE: Hyperparameter Optimization (HPO) is skipped here for brevity.
# In a real-world ML task, HPO is arguably the most critical step regarding 
# model performance and should be performed at this stage.

# ============================================================================== #
# 2.1 MODEL EVALUATION (In-Sample) ----
# ============================================================================== #

print("--- Evaluating Model Performance ---")

# TODO: Generate predictions for the calculation of AUC.
# Hint: We need probabilities, not class labels.
preds <- catboost.predict(model_cat, train_pool, prediction_type = "_______")

# TODO: Calculate AUC using the pROC package.
# Pass the true labels (y) and your predicted probabilities.
auc_score <- pROC::auc(_______, _______)
print(paste("In-sample AUC:", round(auc_score, 4)))

# NOTE ON EVALUATION:
# We are calculating the AUC on the same data used for training (In-Sample).
# In a real-world project, this is BAD PRACTICE as it leads to over-optimistic results.
# You must always perform Cross-Validation (CV) or use a hold-out Test Set 
# to get a reliable estimate of your model's predictive performance.

# ============================================================================== #
# 3. MANUAL SHAP ANALYSIS (EDUCATIONAL) ----
# ============================================================================== #

print("--- Manual SHAP Calculations ---")

# 1. Extract raw SHAP values from CatBoost
# The output is a matrix: Features are cols 1 to N, last col is Bias
raw_shap <- catboost.get_feature_importance(model_cat, train_pool, type = "ShapValues")

# 2. Separate Contributions matrix and Bias vector
# Hint: In CatBoost output, the Bias (Base Value) is located in the LAST column.
# The Features are in columns 1 to (N-1).

# TODO: Subset the matrix to get only feature contributions
shap_contribs <- raw_shap[, _______] 

# TODO: Extract the Bias (It is the same for all rows, take the first value of the last col)
shap_bias <- raw_shap[1, _______] 

# Assign names to the matrix columns for easier handling
colnames(shap_contribs) <- colnames(X)

# --- A. Manual Feature Importance Plot ---
# TODO: Calculate Global Importance.
# Definition: check lecture slides ...
imp_df <- data.frame(
  Feature = colnames(shap_contribs),
  Importance = colMeans(_______(shap_contribs)) 
)

p_manual_imp <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = col_decrease) +
  coord_flip() +
  labs(title = "Manual Feature Importance", x = "", y = "mean(|SHAP value|)")
# TODO: Check the help for reorder(...)


print(p_manual_imp)

# --- B. Manual Dependence Plot (Ad Spend) ---
# Logic: Scatter plot of Feature Value vs SHAP Value
feature_name <- "Ad spend"
dep_df <- data.frame(
  Feature_Value = X[[feature_name]],
  # TODO: Extract the SHAP values for the specific feature
  SHAP_Value = shap_contribs[, _______]
)

p_manual_dep <- ggplot(dep_df, aes(x = Feature_Value, y = SHAP_Value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE, color = col_increase) +
  labs(title = paste0("Manual Dependence: ", feature_name), 
       subtitle = "Shows spurious correlation",
       x = paste0(feature_name, " (Feature)"), y = "SHAP Contribution")

print(p_manual_dep)

# TODO: Change the 'feature_name' variable above to other features (e.g., "Economy", 
# "Sales calls") and re-run this block. Investigate how the relationship between 
# the feature value and the SHAP contribution changes for different inputs.

# --- C. Manual Microscopic Explanation (Obs #1) ---
# Logic: Bar chart showing contributions for a single row
obs_idx <- 1

# TODO: Subset the `shap_contribs` to obtain the values you need for a 
# microscopic explanation
micro_df <- data.frame(
  Feature = colnames(shap_contribs),
  Contribution = shap_contribs[_______, _______]
)

# TODO: Create a "Type" column. 
# It should be "Increase" if Contribution > 0, otherwise "Decrease".
micro_df$Type <- ifelse(micro_df$Contribution > 0, "_______", "_______")

p_manual_micro <- ggplot(micro_df, aes(x = reorder(Feature, abs(Contribution)), 
                                       y = Contribution, fill = Type)) +
  geom_col() +
  coord_flip() +
  # Use consistent colors: Red for Increase, Blue for Decrease
  scale_fill_manual(values = c("Increase" = col_increase, 
                               "Decrease" = col_decrease)) +
  labs(title = paste0("Manual Breakdown: Obs #", obs_idx),
       # TODO: Complete the equation in the subtitle.
       # Prediction = Bias + Sum of Contributions
       subtitle = paste0("Bias: ", round(shap_bias, 2), 
                         " + Sum(Contribs): ", round(sum(_______), 2),
                         " = Prediction: ", round(shap_bias + sum(micro_df$Contribution), 2)))

print(p_manual_micro)

# ============================================================================== #
# 4. CREATE SHAPVIZ OBJECT ----
# ============================================================================== #

print("--- Creating Shapviz Object ---")

# We use the manually extracted parts from Section 3 to build the object
shap_obj_cat <- shapviz(shap_contribs, X = X, baseline = shap_bias)

# ============================================================================== #
# 5. SHAPVIZ MICROSCOPIC PLOTS (Waterfall & Force) ----
# ============================================================================== #

print("--- Generating Waterfall and Force Plots ---")

# Define color vector for shapviz (Negative first, Positive second)
sv_cols <- c(col_increase, col_decrease)

# Select two interesting observations
obs_indices <- c(1, 10)

# --- A. Waterfall Plot ---
# TODO: Generate a waterfall plot for the first observation index
# Check the shapviz manual to find the function that produces waterfall plots
p_waterfall <- _______(shap_obj_cat, row_id = obs_indices[1]) +
  ggtitle(paste0("Waterfall Plot: Obs #", obs_indices[1])) +
  scale_fill_manual(values = sv_cols)

print(p_waterfall)

# --- B. Force Plot ---
# Similar to waterfall but condensed into a single bar/stack
# TODO: Generate a force plot for the first observation index
# Check the shapviz manual to find the function that produces force plots
p_force <- _______(shap_obj_cat, row_id = obs_indices[1]) +
  ggtitle(paste0("Force Plot: Obs #", obs_indices[1])) +
  scale_fill_manual(values = sv_cols)

print(p_force)

# Compare two observations side-by-side using patchwork
p_comparison <- (sv_waterfall(shap_obj_cat, row_id = obs_indices[1]) + 
                   scale_fill_manual(values = sv_cols)) + 
  (sv_waterfall(shap_obj_cat, row_id = obs_indices[2]) + 
     scale_fill_manual(values = sv_cols)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

print(p_comparison)

# ============================================================================== #
# 6. SHAPVIZ GLOBAL PLOTS ----
# ============================================================================== #

print("--- Generating Global Plots ---")

# A. Importance Plot (Beeswarm)
# TODO: Create a Beeswarm plot using `sv_importance` and setting kind = "bee"
p_imp_bee <- _______(shap_obj_cat, kind = "_______") + 
  ggtitle("CatBoost: Feature Importance (Beeswarm)") +
  scale_color_gradient(low = col_decrease, high = col_increase)

p_imp_bee

# Importance Plot (Bar)
p_imp_bar <- sv_importance(shap_obj_cat, kind = "bar", fill = col_decrease) + 
  ggtitle("CatBoost: Feature Importance (Bar)")

p_imp_bar

# B. Dependence Plots (Grid)
p_dep <- sv_dependence(shap_obj_cat, 
                       v = colnames(X), 
                       color_var = NULL) +
  scale_color_gradient(low = col_decrease, high = col_increase) +
  plot_layout(ncol = 3)

p_dep

p_dep_ylim_fixed <- sv_dependence(shap_obj_cat, 
                       v = colnames(X), 
                       color_var = NULL, 
                       ylim = range(shap_contribs)) +
  scale_color_gradient(low = col_decrease, high = col_increase) +
  plot_layout(ncol = 3)
p_dep_ylim_fixed

# TODO: Discuss why the additional argument `ylim = range(shap_contribs)` is 
# worth being added. 