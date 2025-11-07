#==============================================================================#
# Naive Bayes Classification                                                   #
# as a benchmark model                                                         #
# Exercise 02: iris data (with solutions)                                      #
#==============================================================================#
rm(list = ls(all.names = TRUE))    # What is happening here?


# 00: packages -----------------------------------------------------------------
library(Hmisc)
library(naivebayes)
library(MASS)

# 01: loading and inspecting the data ------------------------------------------
data(iris, package = "datasets")
?iris  # read the data documentation to learn about the data set

# 02: descriptive data analysis ------------------------------------------------

# descriptive stats ----
describe(iris)

# visualisation: scatter plot matrix  ----
pairs(iris[1:4], col = iris$Species)

# Discuss based on the scatter plot matrix, if the conditional independence 
# assumption might be valid or not

# Q: what classification quality do you expect regarding NB, LDA and QDA?
# A: since there are strong (linear) correlation between the variables obvious,
#    we expect LDA/QDA working better because they don't assume the obviously 
#    violated conditional independence as NB

# 02: NB assuming normally distributed variables -------------------------------

nb_01 <- naive_bayes(Species ~ ., data = iris)
summary(nb_01)

# let's have a look a the estimated densities 
par(mfrow = c(2, 2))
plot(nb_01)
par(mfrow = c(1, 1))

# these are the estimated density parameters
tables(nb_01)

# How good is the classification?
table(iris$Species, predict(nb_01), dnn = c("obs", "pred"))

# Which data points are difficult to be predicted correctly?
# large red points are misclassified
correct_pred_nb_01 <- predict(nb_01) == iris$Species
pairs(iris[1:4], col = 2 - correct_pred_nb_01, pch = 20,
      cex = 2 - correct_pred_nb_01)


# 03: Relaxation of normality assumption ---------------------------------------

# now we relax the normality assumption and use a kernel smoother to estimate
# the underlying univariate density functions
nb_02 <- naive_bayes(Species ~ ., usekernel = TRUE, data = iris)

# check the model outcome by repeating the steps in section 02
summary(nb_02)

# let's have a look a the estimated densities 
par(mfrow = c(2, 2))
plot(nb_02)
par(mfrow = c(1, 1))

# these are the estimated density parameters
tables(nb_02)

# How good is the classification?
table(iris$Species, predict(nb_02), dnn = c("obs", "pred"))

# Which data points are difficult to be predicted correctly?
# large red points are misclassified
correct_pred_nb_02 <- predict(nb_02) == iris$Species
pairs(iris[1:4], col = 2 - correct_pred_nb_02, pch = 20,
      cex = 2 - correct_pred_nb_02)

  
# it seems the normality assumption wasn't too bad, but maybe the density
# estimates are a little too wiggly; 
# we can control this by using the additional 'adjust' argument to increase 
# the bandwidth of the kernel estimator.

?density   # read the documentation to understand what 'adjust' does
nb_03 <- naive_bayes(Species ~ ., usekernel = TRUE, adjust = 1.5, data = iris)

summary(nb_03)

# let's have a look a the estimated densities 
par(mfrow = c(2, 2))
plot(nb_03)
par(mfrow = c(1, 1))

# these are the estimated density parameters
tables(nb_03)

# How good is the classification?
table(iris$Species, predict(nb_03), dnn = c("obs", "pred"))

# Which data points are difficult to be predicted correctly?
# large red points are misclassified
correct_pred_nb_03 <- predict(nb_03) == iris$Species
pairs(iris[1:4], col = 2 - correct_pred_nb_03, pch = 20,
      cex = 2 - correct_pred_nb_03)

# what does change due to the less wiggly density estimations?


# Which data point is now predicted better?
better_now <- correct_pred_nb_03 & !correct_pred_nb_03
which(better_now)
pairs(iris[1:4], col = better_now +1, pch = 20,
      cex = better_now + 1)

# 04: LDA ----------------------------------------------------------------------
# Now perform an LDA analysis as discussed in ML1
lda_01 <- lda(Species ~ .,data = iris)

# How good is the classification?
table(iris$Species, predict(lda_01)$class, dnn = c("obs", "pred"))

# Which data points are difficult to be predicted correctly?
# large red points are misclassified
correct_pred_lda_01 <- predict(lda_01)$class == iris$Species
pairs(iris[1:4], col = 2 - correct_pred_lda_01, pch = 20,
      cex = 2 - correct_pred_lda_01)

# 05: QDA ----------------------------------------------------------------------
# Now perform an QDA analysis as discussed in ML1
qda_01 <- qda(Species ~ .,data = iris)
# How good is the classification?
table(iris$Species, predict(qda_01)$class, dnn = c("obs", "pred"))

# Which data points are difficult to be predicted correctly?
# large red points are misclassified
correct_pred_qda_01 <- predict(qda_01)$class == iris$Species
pairs(iris[1:4], col = 2 - correct_pred_qda_01, pch = 20,
      cex = 2 - correct_pred_qda_01)

# 06: Genereal discussion ------------------------------------------------------  

# Q: Comparing the QDA, LDA and NB outcome shows that LDA works ________ 
# (better/worse) for the 'iris' data set. Discuss what this means regarding 
# the correlation structure of the explanatory variables.

# A:  ....

