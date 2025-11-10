library(e1071)


set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c( rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y, main = "Données simulées")



train <- sample(200,100)
svmfit_rad_01 <- svm(y~. , data = dat[train, ], kernel = "radial", 
              gamma = 1, cost = 1)
svmfit_rad_02 <- svm(y~. , data = dat[train, ], kernel = "radial", 
              gamma = 1, cost = 1e-05)
svmfit_lin <- svm(y~. , data = dat[train, ], kernel = "linear", scale = TRUE, 
                cost = 1)
svmfit_pol <- svm(y~. , data = dat[train, ], kernel = "polynomial", 
                scale = TRUE, degree =2, cost = 1)

summary(svmfit_01)
summary(svmfit_02)
# ---- Visualisation des modèles----
plot(svmfit_lin, dat[train, ], main = "SVM linéaire sur les données d'entraînement")
plot(svmfit_pol, dat[train, ], main = "SVM linéaire sur les données d'entraînement")
plot(svmfit_rad_01, dat[train, ], main = "SVM linéaire sur les données d'entraînement")

set.seed(1)
tune.out <- tune(svm, y~. , data = dat[train, ],
                kernel = "radial",
                ranges = list(
                  cost = c(0.1, 1, 10, 100, 1000), 
                  gamma = c(0.5, 1, 2, 3, 4))
                )
summary(tune.out)          

table(
  true = dat[-train, "y"],
  pred = predict(
    tune.out$best.model, newdata = dat[-train, ]
  )
    )

