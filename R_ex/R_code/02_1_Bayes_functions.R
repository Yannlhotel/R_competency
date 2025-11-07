library(ggplot2) # for visualisation
library(MASS) # for LDA and QDA
library(pROC)
#install.packages("here")
library(here)
load("Diabetes.Rda")

head(Diabetes)
str(Diabetes)

pi0 <- 0.5
mu0 <- 4
mu1 <- 5
sigma <- 1

phi0 <- function(x, mu, sigma) { dnorm(x, mean = mu, sd = sigma) }
phi1 <- function(x, mu, sigma) { dnorm(x, mean = mu, sd = sigma) }


x <- seq(0, 8, length.out = 100)
plot(x, phi0(x, mu= mu0, sigma = sigma), type="l", col="blue", lwd=2, ylab="Density", xlab="x")
lines(x,phi1(x, mu= mu1, sigma = sigma), col="red", lwd=2)
legend("topright", legend=c("phi0", "phi1"), col=c("blue", "red"), lwd=2)


posterior <- function(x, pi0=0.5, mu0=4, mu1=5, sigma=1) {
  phi0_x <- phi0(x, mu=mu0, sigma=sigma)
  phi1_x <- phi1(x, mu=mu1, sigma=sigma)
  return((1 - pi0) * phi1_x / (pi0 * phi0_x + (1 - pi0) * phi1_x))
}
posterior(4.5)
plot(x, 1- posterior(x), type="l", col="blue", lwd=2, ylab="Density", xlab="x")
lines(x, posterior(x), col="red", lwd=2)
legend("topright", legend=c("pi0(x)", "pi1(x) _ (posterior)"), col=c("blue", "red"), lwd=2)

posterieor(4.5)


library(ggplot2)
ggplot() +
  geom_function(fun = posterior,
                args = list(pi0 = 0.5,
                            mu0 = 4,
                            mu1 = 5,
                            sigma = 1)) +
  scale_x_continuous(limits = c(0, 8))
# with base R graphics this can be achieved via
curve(posterior(x, pi0 = 1 - posterior(x), mu0 = 4, mu1 = 5, sigma = 1),
      from = 0, to = 8)

