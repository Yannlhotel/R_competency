library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)

# Load dataset
file_path <- "C:/Users/yannl/Documents/BHT/ML2/R_project/Data/calories.csv"
df <- read.csv(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Shape and first overview
cat("Dataset shape :", nrow(df), ncol(df))
print(head(df, 10))
print(str(df))

set.seed(123)  # pour reproductibilité

df_1000 <- df[sample(nrow(df), 1000), ]

# Enregistrer sur disque
saveRDS(df_1000, file = "C:/Users/yannl/Documents/BHT/ML2/R_project/df_1000.rds")
write.csv(df_1000, "df_1000.csv", row.names = FALSE)


# ---- 1. Select only numeric columns ----
num_df <- df %>% select_if(is.numeric)

# ---- 2. Compute correlation matrix ----
corr_matrix <- round(cor(num_df, use = "complete.obs"), 2)

# Convert to long format
corr_df <- melt(corr_matrix)

# ---- 3. Correlation matrix heatmap ----
p_corr <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white", size = 3) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 4. Histogram of calories ----
p_hist <- ggplot(df, aes(x = Calories)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Histogram of Calories", x = "Calories", y = "Count")

# ---- 5. Display both plots as subplots ----
p_hist + p_corr


# Plot 1: Duration vs Calories
p1 <- ggplot(df, aes(x = Duration, y = Calories)) +
  geom_point(color = "steelblue") +
  theme_minimal() +
  labs(title = "Calories vs Duration", x = "Duration (min)", y = "Calories")

# Plot 2: Heart_Rate vs Calories
p2 <- ggplot(df, aes(x = Heart_Rate, y = Calories)) +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(title = "Calories vs Heart Rate", x = "Heart Rate (bpm)", y = "Calories")

# Plot 3: Body_Temp vs Calories
p3 <- ggplot(df, aes(x = Body_Temp, y = Calories)) +
  geom_point(color = "darkgreen") +
  theme_minimal() +
  labs(title = "Calories vs Body Temperature", x = "Body Temperature (°C)", y = "Calories")

# Plot 4: Weight vs Calories
p4 <- ggplot(df, aes(x = Weight, y = Calories)) +
  geom_point(color = "purple") +
  theme_minimal() +
  labs(title = "Calories vs Weight", x = "Weight (kg)", y = "Calories")

# ---- 2x2 Layout ----
(p1 + p2) /
  (p3 + p4)


df_1000 <- readRDS("df_1000.rds")
dim(df_1000)
head(df_1000)
str(df_1000)

