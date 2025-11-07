#==============================================================================#
# Diabetes Classification: LDA / QDA / Logistic                                 #
#==============================================================================#

# 00: Packages -----------------------------------------------------------------
library(MASS)
library(pROC)
library(glmnet)
library(package)
library(dplyr)

# 01: Load Data ----------------------------------------------------------------
load("C:/Users/yannl/Documents/BHT/ML2/R_Code/Diabetes.Rda")
str(Diabetes)

# 02: Train/Test Split --------------------------------------------------------
set.seed(50)
n <- nrow(Diabetes)
testidx <- sample(n, 2000)
test <- Diabetes[testidx, ]
train <- Diabetes[-testidx, ]

# 03: Fit Models --------------------------------------------------------------

# LDA Age
lda.fit1 <- lda(YN ~ Age, data=train)
pr1 <- predict(lda.fit1, newdata=test)
roc1 <- roc(test$YN, pr1$posterior[,2])

# LDA BMI
lda.fit2 <- lda(YN ~ BMI, data=train)
pr2 <- predict(lda.fit2, newdata=test)
dim(pr2)
roc2 <- roc(test$YN, pr2$posterior[,2])

# LDA Age + BMI
lda.fit3 <- lda(YN ~ Age + BMI, data=train)
pr3 <- predict(lda.fit3, newdata=test)
roc3 <- roc(test$YN, pr3$posterior[,2])

# QDA Age + BMI
qda.fit <- qda(YN ~ Age + BMI, data=train)
pr.qda <- predict(qda.fit, newdata=test)
roc.qda <- roc(test$YN, pr.qda$posterior[,2])

# Logistic Regression Age + BMI
glm.fit <- glm(YN ~ Age + BMI, data=train, family=binomial)
pr.glm <- predict(glm.fit, newdata=test, type="response")
roc.glm <- roc(test$YN, pr.glm)

# 04: Compare ROC curves ------------------------------------------------------
roc_list <- list(
  "LDA Age" = roc1,
  "LDA BMI" = roc2,
  "LDA Age+BMI" = roc3,
  "QDA Age+BMI" = roc.qda,
  "Logistic Age+BMI" = roc.glm
)

# Plot all ROC curves in one graph
colors <- c("blue", "red", "green", "purple", "orange")
ggroc(roc_list, aes="color") +
  scale_color_manual(values=colors) +
  ggtitle("Comparison of ROC curves") +
  theme_minimal() +
  theme(legend.title = element_blank())

# 05: Print AUC values --------------------------------------------------------
sapply(roc_list, auc)

