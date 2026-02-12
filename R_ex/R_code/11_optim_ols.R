#==============================================================================#
# Exercise sheet 09: NN II - numeric solution instead of OLS                   #
# Code template for exercise 03                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: simulate data ------------------------------------------------------------
set.seed(20241222)   # This seed might be a hint. Keep searching ...

# simulate a data set for cubic regression 
n <- 25
x <- runif(n, min = -1, max = 1)
resid <- rnorm(n, mean = 0, sd = sqrt(0.2))
y <- 10 + 4 * x - 3 * x^2 + resid
plot(x, y)


# define a function which gives the fitted values as a function of
# the beta parameters and the data x
f_hat <- function(x, beta) beta[1] + beta[2] * x + beta[3] * x^2

##define the loss function
loss <- function(beta, x, y) sum((y - f_hat(x, beta))^2)

##define the partial derivatives
dlossdb1 <- function(beta, x, y) ???
dlossdb2 <- function(beta, x, y) ???
dlossdb3 <- function(beta, x, y) ???

beta <- c(0,0,0)
loss(beta,x,y)
s <- 0.01
beta_new <- rep(NA, 3)
plot(x, y)
for(i in 1:200){
  gradloss <- c(dlossdb1(beta, x, y), 
                dlossdb2(beta, x, y),
                dlossdb3(beta, x, y))
  beta_new[1] <- beta[1] - s * gradloss[1]
  beta_new[2] <- beta[2] - s * gradloss[???]
  beta_new[3] <- beta[???] - s * gradloss[???]
  beta <-   beta_new
  
  # you can uncomment the next line to see all intermediate solutions
  # lines(sort(x), f_hat(sort(x), beta), col = "red", lty = 2)
}
lines(sort(x), f_hat(sort(x), beta), col = "black", lty = 1, lwd = 2)
beta
loss(beta, x, y)

## repeat if necessary

# the true values from the regression (which uses an exact fitting method) are
lm(???)
