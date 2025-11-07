#==============================================================================#
# Exercise sheet 04: Support Vector Classifiers                                #
# Code template for exercise 02                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
library(ISLR2)
library(e1071)
library(pROC)
library(naivebayes)

# 01: load data ----------------------------------------------------------------
data("College", package = "ISLR2")
College
# 02: descriptive stats and data preparation -----------------------------------
dim(College)
table(College$Private)
summary(College)

# From previous exercises we know that some variables were very skewed.
# E.g. Accept
hist(College$Accept)

# It is easier is we define new variables with the log data for four variables
# and append them to the data frame College before we create training and test 
# data sets
College$lAccept <- log(College$Accept)
College$lApps <- log(College$Apps)
College$lF.Undergrad <- log(College$F.Undergrad)
College$lExpend <- log(College$Expend)

# Create training and test data set with 80/20 split
set.seed(2)
train_share <- 0.8
train_idx <- sample(1:nrow(College), size = as.integer(nrow(College) * 0.8))
Colltrain <- College[train_idx, ]
Colltest <- College[-train_idx, ]

# 03: simple 2-variable model --------------------------------------------------
# first SVM model on two variables
svm_01 <- svm(Private ~ lAccept + PhD, data = Colltrain,  
               kernel = "linear", cost = 0.1, scale = FALSE)
# visualisation
plot(svm_01, Colltrain, lAccept ~ PhD)

# model summary
summary(svm_01)

# obtain the confusion matrix (training data)
ypred <- predict(svm_01, newdata = Colltrain)
table(ypred, Colltrain$Private)

# and the proportion of the correctly predicted values (accuracy)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

# make sure you understand why this gives the desired result

# 04: 3-variable model ---------------------------------------------------------
# except an additional variable all other settings remain the same
svm_02 <- svm(Private ~ lAccept + PhD + lExpend, data = Colltrain, kernel = "linear",cost = 0.1, scale = FALSE)
plot(svm_02, Colltrain, lAccept ~ PhD)
summary(svm_02)
# Read the notes in the worksheet about this diagram
mean(Colltrain$lExpend)

# Specify that the underlying value for lExpend should be the approx mean value 
plot(svm_02, Colltrain, lAccept ~ PhD, slice = list(lExpend = 9))

# Recap: Why do we use the value `lExpend = 9`?

# Try out the above command with different "slice values"
plot(svm_02, Colltrain, lAccept ~ PhD, slice = list(lExpend = 7))
plot(svm_02, Colltrain, lAccept ~ PhD, slice = list(lExpend = 11))

# Further model diagnostics
summary(svm_02)
ypred <- predict(svm_02, newdata = Colltrain)
table(ypred, Colltrain$Private)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

# better than before? Yes

# 05: full model ---------------------------------------------------------------
# Now try all the variables available used in the GAM exercise
mod_full <- Private ~ lAccept + lApps + lF.Undergrad + Room.Board + lExpend + 
  S.F.Ratio + PhD

svm_03 <- svm(mod_full, Colltrain,kernel = "linear", cost = 0.1, scale = FALSE)

# That warning is not good

# We'll try scaling the data before running (which is the default setting)
svm_04 <- svm(mod_full, Colltrain,kernel = "linear", cost = 0.1, scale = TRUE)

# model diagnostics
summary(svm_04)
ypred <- predict(svm_04, newdata = Colltrain)
table(ypred, Colltrain$Private)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

# Actually the predictions are very similar whether scaling is used or not, 
# but we should always be wary of a WARNING: reaching max number of iterations

# 05: CV for optimal costs -----------------------------------------------------
# now investigate the best cost value using cross validation
tune_01 <- tune(svm, mod_full, data = Colltrain, kernel = "linear", 
                scale = TRUE,
                ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 50)))
# Further model diagnostics
summary(tune_01)  # error is the smallest when c = 0,1 or 0,5
ypred <- predict(tune_01$best.model, newdata = Colltrain)
table(ypred, Colltrain$Private)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

best_svm <- tune_01$best.model
summary(best_svm)
ypred <- predict(best_svm, Colltrain)
table(ypred, Colltrain$Private)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

# the accuracy on the training data best model is slightly better


# 06: ROC analysis -------------------------------------------------------------
# We need to re-run the SVM so that the algorithm returns probabilities for how 
# likely a college private is using the 'prob = TRUE' argument
svm_05 <- svm(mod_full, data = Colltrain, prob = TRUE,
              kernel = "linear",scale = TRUE )

# two steps required to extract the probabilities 
ypred <- predict(svm_05, Colltrain, prob = TRUE)  
# since the probs are stored as án attribute, we need the next lineof code
ypredp <- attr(ypred, "probabilities")[, "Yes"]

# create a ROC object and plot it
roc_obj_train  <-  roc(Colltrain$Private, ypredp)
ggroc(roc_obj_train)
auc(roc_obj_train)

# 07: model evaluation on test data --------------------------------------------
ypred <- predict(svm_05, newdata = Colltrain)
table(ypred, Colltrain$Private)
sum(diag(prop.table(table(ypred, Colltrain$Private))))

ypred_test <- predict(svm_05, Colltest, prob = TRUE)
ypred_testp <- attr(ypred_test, "probabilities")[,1]
roc_obj_test  <-  roc(Colltest$Private, ypred_testp)
ggroc(list(train = roc_obj_train, test = roc_obj_test))
auc(roc_obj_train)
auc(roc_obj_test)

# 08: Naive Bayes classification -----------------------------------------------
nb_01 <- naive_bayes(mod_full, data = Colltrain, usekernel = TRUE)

summary(nb_01)
plot(nb_01, "lAccept")
plot(nb_01, "PhD")
plot(nb_01, "lExpend")


# predictions on test data
ypred_nb_test_class <- predict(nb_01, newdata = Colltest)

# for ROC curves we need probabilities instead of classes
ypred_nb_test <- predict(nb_01, newdata = Colltest, type = "prob")[, "Yes"]
roc_obj_nb_test  <-  roc(Colltest$Private, ypred_nb_test)
ggroc(list(train = roc_obj_train, 
           test = roc_obj_test,
           test_nb = roc_obj_nb_test))
auc(roc_obj_test)
auc(roc_obj_nb_test)

# accuracy NB:
sum(diag(prop.table(table(ypred_nb_test_class, Colltest$Private))))

# 09: conclusion ---------------------------------------------------------------

# what was it all about?
