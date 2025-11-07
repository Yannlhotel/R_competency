library(ggplot2) # for visualisation
library(MASS) # for LDA and QDA
library(pROC)
install.packages("here")
library(here)
load(here("R_Code", "Diabetes.Rda"))

head(Diabetes)
str(Diabetes)

phi0 <- function(x) {
  1/sqrt(2*pi)*exp(-(x-4)**2/2)
}


phi1 <- function(x) {
  1/sqrt(2*pi)*exp(-(x-5)**2/2)
}


x <- seq(0, 8, length.out = 100)

plot(x, phi0(x), type="l", col="blue", lwd=2, ylab="Density", xlab="x")
#plot(x, phi1(x), type="l", col="red", lwd=2, ylab="Density", xlab="x")
lines(x, phi1(x), col="red", lwd=2)
legend("topright", legend=c("phi0", "phi1"), col=c("blue", "red"), lwd=2)

pi0 <- function(x) {
  1/(1+exp(-(9-2*x)/2))
}
posterieor <- function(x) {
  1/(1+exp((9-2*x)/2))
}

plot(x, pi0(x), type="l", col="blue", lwd=2, ylab="Density", xlab="x")
#plot(x, phi1(x), type="l", col="red", lwd=2, ylab="Density", xlab="x")
lines(x, posterieor(x), col="red", lwd=2)
legend("topright", legend=c("pi0", "posterieor"), col=c("blue", "red"), lwd=2)

posterieor(4.5)
