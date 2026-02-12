#==============================================================================#
# Exercise sheet 08: NN I - the basics                                         #
# Code template for exercise 02                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: load data ----------------------------------------------------------------
load("Data/NN_I_regr_data.Rda")

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
  ???
} 

# plot the function, so you know what it looks like!
curve(sigmoid, -5, 5)

# Simple neural network
NN2 <- function(param, x1sc, x2sc) {
  # This function is the neural network
  # Input data is x1 and x2
  # all weights are packed in one param vector 
  
  # unpack the param vector
  whl11 <- param[["whl11"]]
  whl12 <- param[["whl12"]]
  bhl1 <- param[["bhl1"]]
  wol1 <- param[["wol1"]]
  whl21 <- param[["whl21"]]
  whl22 <- param[["whl22"]]
  bhl2 <- param[["bhl2"]]
  wol2 <- param[["wol2"]]
  bol <- param[["bol"]]
  
  # hidden layer
  z1 <- ??? * x1sc + ??? * x2sc + ???
  z2 <- whl21 * ??? + whl22 * ??? + ???
  
  # activation
  a11 <- ???
  a12 <- ???
  
  # output layer
  a <- wol1 * ??? + wol2 * ??? + bol
  
  return(a)
}  

# 04: parameter estimation -----------------------------------------------------

# To be able to run the whole process with one keyboard short (Ctrl + Enter),
# it is combined to one expression using `{...}`
{
  # number of iterations
  niter <- 10
  
  # initialise parameters (small non-zero and not equal)
  params <- c(whl11 = 0.010,
              whl12 = -0.011,
              bhl1 = 0.012,
              wol1 = -0.012,
              whl21 = 0.010,
              whl22 = -0.011,
              bhl2 = 0.012,
              wol2 = -0.012,
              bol = 0.501)
  
  # Initialisation of MSE criteria
  bestMSE <- Inf
  
  # further parameters
  n <- length(x1sc)
  window <- 1
  
  for(iter in 1:niter){
    
    # We update parameter in turn and select the update if it gives a lower MSE   
    Delta <- rep(0, ???)  # initialise the change vector
    
    # define which parameter to perturb
    j <-  (iter %% length(params)) + 1
    
    # for later (part 6) reduce the window size incrementally
    # window <- 0.1 + 1/iter
    
    Delta[j] <- rnorm(1, 0, window) #generate the change in parameter and assign it
    
    # define the new parameter vector 
    params_perturbed <- params + Delta
    
    #  fitted <- rep(NA,n)
    # call NN2 for each observation
    fitted <- NN2(params_perturbed, x1sc, x2sc)
    # loss function
    MSE <- mean((???)^2)
    
    # Improvement?
    # if MSE is better, then update 
    if(MSE ??? bestMSE){
      params <- params_perturbed 
      bestMSE <- MSE
      best.fitted <- fitted
      
    }
    
  }
  cat(niter," iterations\n")
  cat ("Training MSE:", round(bestMSE, 5),"\n")
  plot(ysc, best.fitted, ylim=c(0,1))
  abline(c(0,1))
  
  val.predicted <- ???(params, x1valsc, x2valsc)
  valMSE <- mean((??? - yvalsc)^2)
  cat ("Validation MSE:", round(valMSE, 5), "\n")
  
}  # `Ctrl` + `Enter` here runs the whole {} block

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
# have to do to examine this systematically?

# 07: Stability ----------------------------------------------------------------

# Re-run the code several times.
# Notice that the validation MSE value varies more than the training MSE.


