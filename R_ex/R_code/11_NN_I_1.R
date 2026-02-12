#==============================================================================#
# Exercise sheet 08: NN I - the basics                                         #
# Code template for exercise 01                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))

message("IMPORTANT: Please read through the Worksheet before you start working through this file!")


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: load data ----------------------------------------------------------------
load("Data/NN_I_regr_data.Rda")
ls()
head(x1)
summary(y)
length(x2)
# créer une grille régulière
x1g <- seq(min(x1), max(x1), length.out = 30)
x2g <- seq(min(x2), max(x2), length.out = 30)

grid <- expand.grid(x1 = x1g, x2 = x2g)
# ajustement lisse
fit <- loess(y ~ x1 + x2)

# prédiction sur la grille
yg <- matrix(
  predict(fit, newdata = grid),
  nrow = length(x1g),
  ncol = length(x2g)
)
persp(
  x1g, x2g, yg,
  theta = 30, phi = 25,
  expand = 0.6,
  col = "lightblue",
  xlab = "x1",
  ylab = "x2",
  zlab = "y",
  main = "Surface plot de y en fonction de x1 et x2"
)
points(
  trans3d(x1, x2, y,
          persp(x1g, x2g, yg)),
  pch = 16,
  col = "red"
)

# 02: data scaling -------------------------------------------------------------

# ranges
x1range <- range(x1)
x2range <- range(x2)
yrange <- range(y)
x1sc <- (x1 - x1range[1])/diff(x1range)
x2sc <- (x2 - x2range[1])/diff(x2range)
ysc <- (y - yrange[1])/diff(yrange)

# scale the validation data
# note that you have to scale the same transformations as for the training data 

x1valsc <- (x1val - x1range[1])/diff(x1range)
x2valsc <- (x2val - x2range[1])/diff(x2range)
yvalsc <- (yval - yrange[1])/diff(yrange)

# number of observations in training data
n <- length(x1sc)

# 03: helper functions ---------------------------------------------------------

# sigmoid function definition
sigmoid <- function(v){ 
  1/(1 + exp(-v)) 
}

# plot the function, so you know what it looks like!
curve(sigmoid, -5, 5)

# Simple neural network
NN <- function(param, x1sc, x2sc) {
  # This function is the neural network
  # Input data is x1 and x2
  # all weights are packed in one param vector 
  
  # unpack the param vector
  whl11 <- param[["whl11"]]
  whl12 <- param[["whl12"]]
  bhl1 <- param[["bhl1"]]
  wol1 <- param[["wol1"]]
  bol <- param[["bol"]]
  
  # hidden layer
  z1 <- whl11 * x1sc + whl12 * x2sc + bhl1
  # activation
  a1 <- sigmoid(z1)
  
  # output layer
  a2 <- wol1 * a1 + bol
  return(a2)
}  

# 04: parameter estimation -----------------------------------------------------

# To be able to run the whole process with one keyboard short (Ctrl + Enter),
# it is combined to one expression using `{...}`
{
  # number of iterations
  niter <- 100000
  
  # initialise parameters (small non-zero and not equal) 
  # we use named vector to avoid positional subsetting
  params <- c(whl11 = 0.010,
              whl12 = -0.011,
              bhl1 = 0.012,
              wol1 = -0.012,
              bol = 0.501)
  
  # Initialisation of MSE criteria
  bestMSE <- Inf
  
  # further parameters
  n <- length(x1sc)
  window <- 1
  
  for(iter in 1:niter){
    
    # We update parameter in turn and select the update if it gives a lower MSE   
    Delta <- rep(0, length(params))  # initialise the change vector
    
    # define which parameter to perturb
    j <-  (iter %% length(params)) + 1
    
    # for later (part 7) reduce the window size incrementally
    # window <- 0.1 + 1/iter
    
    # generate a random change in parameter and assign it
    Delta[j] <- rnorm(1, mean = 0, sd = window) 
    
    # define the new parameter vector 
    params_perturbed <- params + Delta
    
    #  fitted <- rep(NA,n)
    # call NN for each observation
    fitted <- NN(params_perturbed, x1sc, x2sc)
    # loss function
    MSE <- mean((fitted - ysc)^2)
    
    # Improvement?
    # if MSE is better, then update 
    if(MSE < bestMSE){
      params <- params_perturbed 
      bestMSE <- MSE
      best.fitted <- fitted
      
    }
    
  }
  cat(niter," iterations\n")
  cat ("Training MSE:", round(bestMSE, 5),"\n")
  plot(ysc, best.fitted, ylim=c(0,1))
  abline(c(0,1))
  
  val.predicted <- NN(params, x1valsc, x2valsc)
  valMSE <- mean((val.predicted - yvalsc)^2)
  cat ("Validation MSE:", round(valMSE,5), "\n")
  
}  # `Ctrl + Enter` here runs the whole {} block

# 05: More iterations ----------------------------------------------------------

# change the number of iterations in line 73 and repeat the main block

# plot the observed and fitted y for the validation data
plot(yvalsc, val.predicted, ylim = c(0, 1))
abline(c(0,1))

# note that these are for the scaled data, the genuine predictions have to be 
# unscaled again
plot(yval, val.predicted * diff(yrange) + yrange[1])
abline(c(0,1))

#The optimal parameters are
params

# 06: Incrementally reducing window width --------------------------------------

# turn the incrementally reducing window width on
# what do you have to change in the code given above?

# Do you get better results with the same amount of iterations? What would you
# have to do to examine this systematically?  Stability of the result.

# 07: Stability ----------------------------------------------------------------

# Re-run the code several times.
# Notice that the validation MSE value varies more than the training MSE.


