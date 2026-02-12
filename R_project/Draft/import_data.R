library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)

# Load our Dataset
# download it from https://www.kaggle.com/datasets/jakewright/200k-youtube-channel-analytics/data
#### PLEASE use your own path
file_path <- "C:/Users/yannl/Documents/BHT/ML2/R_project/Data/all_youtube_analytics.csv"
youtube_data <- read.csv(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Shape and first overview
cat("Dataset shape :", nrow(youtube_data), ncol(youtube_data))
print(head(youtube_data, 10))  # first rows
print(str(youtube_data))  # See how it's build


# We delete some unusefull feature (most of the time 0)
print(summary(youtube_data))
cols_to_drop <- c(
  "annotationClickThroughRate",
  "annotationCloseRate",
  "annotationImpressions",
  "annotationClickableImpressions",
  "annotationClosableImpressions",
  "annotationClicks",
  "annotationCloses",
  "cardClickRate",
  "cardTeaserClickRate",
  "cardImpressions",
  "cardTeaserImpressions",
  "cardClicks",
  "cardTeaserClicks"
)

youtube_clean <- youtube_data[, !(names(youtube_data) %in% cols_to_drop)]

cat("Dataset shape :", nrow(youtube_clean), ncol(youtube_clean))
print(summary(youtube_clean))



# We just keep the most watched video
min_views <- 2250
youtube_interesting <- subset(
  youtube_clean,
  views >= min_views 
)
cat("Dataset shape :", nrow(youtube_interesting), ncol(youtube_interesting))
print(summary(youtube_interesting))

# Create an engagement score
youtube_interesting$engagement <- 
  youtube_interesting$likes +
  youtube_interesting$comments +
  youtube_interesting$shares




# --- Distribution of subscribers gained ---
p_dist <- ggplot(youtube_interesting, aes(x = subscribersGained)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  ggtitle("Distribution of Subscribers Gained")

# --- Correlation heatmap ---
numeric_data <- youtube_interesting %>% select(where(is.numeric))
corr_matrix <- cor(numeric_data, use = "complete.obs")
corr_melt <- melt(corr_matrix)

p_corr <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Correlation Heatmap")

# --- First subplot: distribution + correlation ---
subplot1 <- p_dist + p_corr
subplot1


# --- Plot 1: Views vs Subscribers Gained ---
p1 <- ggplot(youtube_interesting, aes(x = views, y = subscribersGained)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  ggtitle("Subscribers Gained vs Views")

# --- Plot 2: Likes vs Subscribers Gained ---
p2 <- ggplot(youtube_interesting, aes(x = likes, y = subscribersGained)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  ggtitle("Subscribers Gained vs Likes")

# --- Plot 3: Estimated Minutes Watched vs Subscribers Gained ---
p3 <- ggplot(youtube_interesting, aes(x = estimatedMinutesWatched, y = subscribersGained)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  ggtitle("Watch Time vs Subscribers Gained")

# --- Plot 4: Videos Added to Playlists vs Subscribers Gained ---
p4 <- ggplot(youtube_interesting, aes(x = videosAddedToPlaylists, y = subscribersGained)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  ggtitle("Playlist Adds vs Subscribers Gained")

# --- Combine them into a 2x2 subplot ---
subplot2 <- (p1 + p2) / (p3 + p4)
subplot2
