#==============================================================================#
# R Code for GBM exercises - Part I                                            #
#==============================================================================#
rm(list = ls(all.names = TRUE))
#install.packages("tidyr")
# EXERCISE I: GBM by hand ------------------------------------------------------

# --- 1. Load Libraries & Prepare Data ----
library(rpart)      # For our weak learner (CART)
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(tidyr)

# Load and clean the data (remove NAs for this simple exercise)
data("airquality")
df <- airquality[, c("Ozone", "Temp")] %>% na.omit()

# --- 2. Set Hyperparameters ----
M <- 100                     # Number of trees (iterations)
eta <- 0.1                   # Our learning rate (eta)
tree_depth <- 1              # Max depth of each tree (a "stump")

# --- 3. Initialize the Model ----
# We start with the simplest possible model, F_0(x).
# As derived in the theoretical exercises for Squared Loss, 
# F_0 is just the mean of y.
F_0 <- mean(df$Ozone)

# 'F_current' will hold the model's predictions at each step m
F_current <- rep(F_0, length(df$Temp))

# We can store the weak learners and predictions for analysis
h_all <- list()                # To store all M trees
F_all <- list()                # To store the predictions at each step
mse <- vector()                # to store the mse at each step
F_all[[1]] <- F_current

# --- 4. Run the GBM Algorithm ----

for (m in 1:M) {
  
  # --- Phase 1: Direction (Calculate Pseudo-Residuals) ---
  # For L2 (Squared) Loss, the pseudo-residual is (y - F_current)
  # TODO: Calculate the pseudo-residuals 'y_tilde'
  
  y_tilde <- df$Ozone - F_all[[m]]
    
    
    # --- Phase 2: Structure (Fit Weak Learner) ---
    # We fit a shallow decision tree (h_m) to predict the residuals.
    # We use a temporary data frame for this.
    df_loop <- data.frame(y_tilde = y_tilde, Temp = df$Temp)
  
  # TODO: Fit an rpart model 'h_m' to predict y_tilde using Temp.
  # Use the 'tree_depth' hyperparameter to control its complexity.
  # HINT: use rpart(..., data = df_loop, control = rpart.control(maxdepth = ...))
  h_m <- rpart(y_tilde ~ Temp, data = df_loop, 
               control = rpart.control(maxdepth = tree_depth))
    
    
    # --- Phase 3: Magnitude & Update (Update F_current) ---
    # In classic GBM (see lecture slide 26), we would do a line search for gamma.
    
    # We CAN simplify that here: Since our weak learner 'h_m' (rpart) is ALSO
    # minimizing L2 loss, its prediction (the leaf-wise mean of y_tilde)
    # is already the optimal L2-minimizing step. We use this prediction
    # directly as our update.
    
    # TODO: Get the predictions from your new tree 'h_m'
    h_m_pred <- predict(h_m)
    
    # TODO: Update the ensemble prediction F_current.
    # Remember to use the learning_rate! (Slide 31: F_m = F_{m-1} + eta * h_m)
    F_current <- F_all[[m]] + eta * h_m_pred
    
    
    # --- Store results for analysis ---
    h_all[[m]] <- h_m
  F_all[[m+1]] <- F_current # Store F_m (F_all[[1]] was F_0)
  
  # Optional: Print the Mean Squared Error (MSE) at this step
  mse <- mean((df$Ozone - F_current)^2)
  cat(paste0("Iteration ", m, ": MSE = ", round(mse, 2), "\n"))
}

cat("GBM Training Complete.\n")

# --- 5. Visualisation ----
m_to_plot <- c(1, 6, 11, 21, 51, M)

# Create a data frame of predictions at different stages
plot_df <- data.frame(
  Temp = df$Temp,
  Ozone = df$Ozone,
  F_all[m_to_plot] |> 
    as.data.frame() |> 
    setNames(paste0("F_", m_to_plot))
)

# Reshape data to long format to fit to ggplot2 functionality
plot_df <- plot_df |> 
  pivot_longer(cols = matches("F_"), names_to = "Iteration", values_to = "F_m") |> 
  mutate(Iteration = gsub("F_", "", Iteration, fixed = TRUE) |> 
           as.integer() |> 
           as.factor(),
         R_m = Ozone - F_m)

# Plot model predictions over time
plot_df |> 
  arrange(Temp) |> 
  ggplot(aes(x = Temp, y = F_m, col = Iteration)) +
  geom_point(aes(x = Temp, y = Ozone), alpha = 0.4, col = "grey40", data = df) +
  geom_step(linewidth = 1) +
  labs(title = bquote("GBM: Model " * F[m](x) * " Improves Over Iterations " * m),
       y = "Ozone") +
  theme_minimal() +
  facet_grid(. ~ Iteration)

# Plot residuals over time
plot_df |> 
  arrange(Temp) |> 
  ggplot(aes(x = Temp, y = R_m, col = Iteration)) +
  geom_hline(yintercept = 0, col = "grey40") +
  geom_point() +
  labs(title = bquote("GBM: residuals " * tilde(y)[m] * " Decrease Over Iterations " * m),
       y = bquote("Resiudals " * tilde(y)[im])) +
  theme_minimal() +
  facet_grid(. ~ Iteration)


# EXERCISE II: Bias-Variance-Trade-Off ----------------------------------------

# --- 1. Load Libraries & Prepare Data ----
# done already in previous exercise


# --- 2. Create Train/Validation Split ----
# We must set a seed to make our split reproducible
set.seed(1234) 
id_train <- sample(1:nrow(df), size = as.integer(0.7 * nrow(df)))
train_df <- df[id_train, ]
valid_df <- df[-id_train, ]

cat(paste("Training samples:", nrow(train_df), "\n"))
cat(paste("Validation samples:", nrow(valid_df), "\n"))

# --- 3. Create a GBM Training Function ----
# The return object of the function is supposed to be a data.frame with
# following structure
mse_results <- data.frame(
  iteration = 0,
  train_mse = 0,
  valid_mse = 0
)



run_gbm <- function(train_data, valid_data, M, eta, tree_depth) {
  
  # TODO: use what you learned in the previous exercise
  
  # --- Initialize Model (on training data) ---
  F_0_train <- mean(train_data$Ozone)
  F_train <- rep(F_0_train, nrow(train_data))
  
  # We must also initialize predictions for the validation set.
  # We use the SAME F_0 (the mean of the training data).
  F_valid <- rep(F_0_train, nrow(valid_data))
  
  # Create a data frame to store the learning curves for both splits
  mse_results <- data.frame(
    iteration = 0,
    train_mse = mean((train_data$Ozone - F_train)^2),
    valid_mse = mean((valid_data$Ozone - F_valid)^2)
  )
  
  # --- Run the GBM Algorithm ---
  
  
  
  
  for (m in 1:M) {
    
    # --- Phase 1: Direction (Calculate Pseudo-Residuals) ---
    # For L2 (Squared) Loss, the pseudo-residual is (y - F_current)
    # TODO: Calculate the training pseudo-residuals 'y_tilde'
    
    y_tilde <- train_data$Ozone - F_train
    
    # --- Phase 2: Structure (Fit Weak Learner) ---
    df_loop <- data.frame(y_tilde = y_tilde, Temp = train_data$Temp)
    
    # Fit an rpart model 'h_m' to predict y_tilde using Temp.
    # Use the 'tree_depth' hyperparameter to control its complexity.
    h_m <- rpart(y_tilde ~ Temp, data = df_loop, 
                 control = rpart.control(maxdepth = tree_depth))
    
    
    # --- Phase 3: Magnitude & Update (Update F_current) ---
    h_m_pred <- predict(h_m)
    
    # Update the ensemble prediction F_current.
    # Get predictions for the training set and update F_train
    F_train <- F_train + eta * h_m_pred
    
    
    # !! CRITICAL -- CRITICAL -- CRITICAL -- CRITICAL -- CRITICAL -- CRITICAL !!
    # Use the SAME(!) tree (h_m) to make predictions on the validation set
    h_m_pred_valid <- predict(h_m, newdata = valid_data)
    F_valid <- F_valid + eta * h_m_pred_valid
    
    # --- Store MSE results ---
    mse_step <- data.frame(
      iteration = m,
      train_mse = mean((train_data$Ozone - F_train)^2),
      valid_mse = mean((valid_data$Ozone - F_valid)^2)
    )
mse_results <- rbind(mse_results, mse_step)
  }
  
  return(mse_results)
}

# --- 4. Define and Run Experiments ----
M <- 100

# Experiment 1: Low depth, Low eta (Baseline)
model_1 <- run_gbm(train_df, valid_df, M, 
                   eta = 0.1, tree_depth = 1)
model_1$model_name <- "eta = 0.1, depth = 1"

# Experiment 2: Low depth, High eta (Fast learning)
model_2 <- run_gbm(train_df, valid_df, M, 
                   eta = 0.5, tree_depth = 1)
model_2$model_name <- "eta = 0.5, depth = 1"

# Experiment 3: High depth, Low eta (Complex model)
model_3 <- run_gbm(train_df, valid_df, M, 
                   eta = 0.1, tree_depth = 5)
model_3$model_name <- "eta = 0.1, depth = 5"

# Experiment 4: High depth, High eta (DANGEROUS model)
model_4 <- run_gbm(train_df, valid_df, M, 
                   eta = 0.5, tree_depth = 5)
model_4$model_name <- "eta = 0.5, depth = 5"


# --- 5. Combine and Plot Results ----
all_results <- rbind(model_1, model_2, model_3, model_4)

# Pivot the data to be 'long' for ggplot
plot_data_long <- all_results %>%
  pivot_longer(
    cols = c("train_mse", "valid_mse"),
    names_to = "dataset_type",
    values_to = "MSE"
  )

# Plot the learning curves!
ggplot(plot_data_long, aes(x = iteration, y = MSE, color = dataset_type)) +
  geom_line(linewidth = 1) +
  # This is the key: facet_wrap separates our 4 models into panels
  facet_wrap(~ model_name) +
  labs(title = "Impact of Hyperparameters on Training vs. Validation MSE",
       x = "Iteration (Number of Trees)",
       y = "Mean Squared Error (MSE)",
       color = "Dataset") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Plot the learning curves!
ggplot(plot_data_long, aes(x = iteration, y = MSE, color = model_name)) +
  geom_line(linewidth = 1) +
  # This is the key: facet_wrap separates our 4 models into panels
  facet_wrap(~ dataset_type, scales = "free_y") +
  labs(title = "Impact of Hyperparameters on Training vs. Validation MSE",
       x = "Iteration (Number of Trees)",
       y = "Mean Squared Error (MSE)",
       color = "Dataset") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

