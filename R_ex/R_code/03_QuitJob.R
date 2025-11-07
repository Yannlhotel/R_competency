library(modeldata)
library(naivebayes)
library(rsample)
library(pROC)
library(ggplot2)

data("attrition", package = "modeldata")

# Conversion des variables en facteurs
attrition$JobLevel <- as.factor(attrition$JobLevel)
attrition$StockOptionLevel <- as.factor(attrition$StockOptionLevel)


set.seed(1)
split_data <- initial_split(attrition, prop = 0.7, strata = "Attrition")
train <- training(split_data)
test <- testing(split_data)

prop.table(table(train$Attrition))
prop.table(table(test$Attrition))


# Make nb prediction
model <- Attrition ~ Age + DailyRate + DistanceFromHome + HourlyRate +
  MonthlyIncome + MonthlyRate
nb_simple <- naive_bayes(model, data = train)
pred_class_simple <- predict(nb_simple, newdata = test)
pred_prob_simple <- predict(nb_simple, newdata = test, type = "prob")


# See results
conf_mat_simple <- table(pred_class_simple, test$Attrition)
conf_mat_simple
sensitivity_simple <- conf_mat_simple["Yes", "Yes"] / sum(conf_mat_simple[, "Yes"])
specificity_simple <- conf_mat_simple["No", "No"] / sum(conf_mat_simple[, "No"])
accuracy_simple <- sum(diag(conf_mat_simple)) / sum(conf_mat_simple)

df_simple <- data.frame(
  sensitivity = sensitivity_simple, 
  specificity = specificity_simple, 
  accuracy = accuracy_simple)
df_simple
# Make new prediction with all the ressources
str(attrition)
nb_tot <- naive_bayes(Attrition ~., data = train)
pred_class_tot = predict(nb_tot, newdata = test)
pred_prob_tot <- predict(nb_tot, newdata = test, type = "prob")

# See the results
conf_mat_tot <- table(pred_class_tot, test$Attrition, dnn = c("pred","obs"))
sensitivity_tot <- conf_mat_tot["Yes", "Yes"] / sum(conf_mat_tot[, "Yes"])
specificity_tot <- conf_mat_tot["No", "No"] / sum(conf_mat_tot[, "No"])
accuracy_tot <- sum(diag(conf_mat_tot)) / sum(conf_mat_tot)
df_tot <- data.frame(
  sensitivity = sensitivity_tot, 
  specificity = specificity_tot, 
  accuracy = accuracy_tot)

df_simple$model <- "NB_simple"
df_tot$model <- "NB_tot"
df_results <- rbind(df_simple, df_tot)
df_results <- df_results[, c("model", "sensitivity", "specificity", "accuracy")]
df_results

roc_simple <- roc(test$Attrition, pred_prob_simple[, "Yes"])
roc_tot <- roc(test$Attrition, pred_prob_tot[, "Yes"])

ggroc(list(
  NB_simple = roc_simple,
  NB_tot = roc_tot
))

library("MASS")


# Make nb prediction
model <- Attrition ~ Age + DailyRate + DistanceFromHome + HourlyRate +
  MonthlyIncome + MonthlyRate
nb_simple <- lda(model, data = train)
pred_simple <- predict(nb_simple, newdata = test)
pred_class_simple <- pred_simple$class
pred_prob_simple  <- pred_simple$posterior


# See results
conf_mat_simple <- table(pred_class_simple, test$Attrition)
conf_mat_simple
sensitivity_simple <- conf_mat_simple["Yes", "Yes"] / sum(conf_mat_simple[, "Yes"])
specificity_simple <- conf_mat_simple["No", "No"] / sum(conf_mat_simple[, "No"])
accuracy_simple <- sum(diag(conf_mat_simple)) / sum(conf_mat_simple)

df_simple_lda <- data.frame(
  sensitivity = sensitivity_simple, 
  specificity = specificity_simple, 
  accuracy = accuracy_simple)
df_simple_lda
# Make new prediction with all the ressources
str(attrition)
nb_tot <- lda(Attrition ~., data = train)
pred_tot = predict(nb_tot, newdata = test)
pred_class_tot <- pred_tot$class
pred_prob_tot  <- pred_tot$posterior
# See the results
conf_mat_tot <- table(pred_class_tot, test$Attrition, dnn = c("pred","obs"))
sensitivity_tot <- conf_mat_tot["Yes", "Yes"] / sum(conf_mat_tot[, "Yes"])
specificity_tot <- conf_mat_tot["No", "No"] / sum(conf_mat_tot[, "No"])
accuracy_tot <- sum(diag(conf_mat_tot)) / sum(conf_mat_tot)
df_tot_lda <- data.frame(
  sensitivity = sensitivity_tot, 
  specificity = specificity_tot, 
  accuracy = accuracy_tot)

df_simple_lda$model <- "LDA_simple"
df_tot_lda$model <- "LDA_tot"
df_results <- rbind(df_results, df_simple_lda, df_tot_lda)
df_results <- df_results[, c("model", "sensitivity", "specificity", "accuracy")]
df_results

roc_simple <- roc(test$Attrition, pred_prob_simple[, "Yes"])
roc_tot <- roc(test$Attrition, pred_prob_tot[, "Yes"])

ggroc(list(
  NB_simple = roc_simple,
  NB_tot = roc_tot
))
