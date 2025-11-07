# check if packages can be loaded, i.e. they are already installed
library(ISLR2)
library(MASS)
library(naivebayes)
library(e1071)
library(pROC)
library(modeldata)
library(rsample)

data("Titanic", package = "datasets")
#??Titanic
titanic_df <- as.data.frame(Titanic)
#View(titanic_df)

repeating_sequence <- rep(1:nrow(titanic_df), times = titanic_df$Freq)
titanic_df <- titanic_df[repeating_sequence, names((titanic_df)) != "Freq"]

stopifnot(sum(Titanic) == nrow(titanic_df))

nb_titanic <- naiveBayes(Survived ~., data = titanic_df)
nb_titanic

priors_titanic <- prop.table(nb_titanic$apriori)

table(titanic_df$Survived) |> prop.table()
table(titanic_df$Survived, titanic_df$Class) |> prop.table(margin = 1)
table(titanic_df$Survived, titanic_df$Sex) |> prop.table(margin = 1)
table(titanic_df$Survived, titanic_df$Age) |> prop.table(margin = 1)
priors_titanic

posterior_titanic_2nd <- priors_titanic * nb_titanic$tables$Class[,"2nd"]
posterior_titanic_2nd

posterior_titanic_2nd/sum(posterior_titanic_2nd)


posterior_titanic_2nd_child <- priors_titanic *
  nb_titanic$tables$Class[, "2nd"] *
  nb_titanic$tables$Age[, "Child"]
posterior_titanic_2nd_child
prop.table(posterior_titanic_2nd_child)

posterior_titanic_3nd_adult <- priors_titanic * nb_titanic$tables$Class[,"3rd"]* nb_titanic$tables$Age[,"Adult"]
prop.table(posterior_titanic_3nd_adult)

nb_pred <- predict(nb_titanic, titanic_df)
#Confusion matrix to check accuracy
conf_mat <- table(nb_pred, titanic_df$Survived)
conf_mat
# sensitivity
conf_mat[2,2]/sum(conf_mat[,2])
#specificity
conf_mat[1,1]/sum(conf_mat[,1])
#misclassification rate (1-accuracy)
1 - sum(diag(conf_mat))/sum(conf_mat)

library(pROC)
pred_prob_nb <- predict(nb_titanic, newdata = titanic_df, type = "raw")
head(pred_prob_nb)
roc_nb <- roc(titanic_df$Survived, pred_prob_nb[, "Yes"])
ggroc(roc_nb)
auc_predict_NB = auc(roc_nb)



logit_01 <- glm(Survived ~ ., data = titanic_df, family = binomial())
pred_prob_logit_01 <- predict(logit_01, newdata = titanic_df,
                              type = "response")
head(pred_prob_logit_01)
roc_logit_01 <- roc(titanic_df$Survived, pred_prob_logit_01)
ggroc(list(NB = roc_nb, Logit = roc_logit_01))
auc_predict_logit_01 = auc(roc_logit_01)

# Modèle avec interactions sexe * âge (femmes et enfants d'abord)
logit_wcf <- glm(Survived ~ Sex * Age + Class, data = titanic_df, family = binomial())

# Prédictions
pred_prob_logit_wcf <- predict(logit_wcf, newdata = titanic_df, type = "response")

# Courbe ROC
roc_logit_wcf <- roc(titanic_df$Survived, pred_prob_logit_wcf)
auc_predict_logit_wcf <- auc(roc_logit_wcf)
# Comparaison graphique
ggroc(list(
  NB = roc_nb,
  Logit = roc_logit_01,
  Logit_WomenChildren = roc_logit_wcf
))

logit_full <- glm(Survived ~ Class * Sex * Age, data = titanic_df, family = binomial())
pred_prob_full <- predict(logit_full, newdata = titanic_df, type = "response")
roc_full <- roc(titanic_df$Survived, pred_prob_full)
auc_predict_logit_full <-auc(roc_full)

ggroc(list(
  NB = roc_nb,
  Logit_simple = roc_logit_01,
  Logit_WomenChildren = roc_logit_wcf,
  Logit_full = roc_full
))

auc_nb <- auc(roc_nb)
auc_logit_01 <- auc(roc_logit_01)
auc_logit_wcf <- auc(roc_logit_wcf)
auc_logit_full <- auc(roc_full)

data.frame(
  Model = c("Naive Bayes", "Logit simple", "Logit Women+Children", "Logit complet"),
  AUC = c(auc_nb, auc_logit_01, auc_logit_wcf, auc_logit_full)
)
