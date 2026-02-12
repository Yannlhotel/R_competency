# Open df
# Be sure that "df_1000.rds" is in your current folder

df_1000 <- readRDS("df_1000.rds")
dim(df_1000)
head(df_1000)
str(df_1000)

set.seed(123)  
n <- nrow(df_1000)

idx_train <- sample(seq_len(n), size = 0.8 * n)

train_df <- df_1000[idx_train, ]
test_df  <- df_1000[-idx_train, ]

# Vérification
dim(train_df)
dim(test_df)