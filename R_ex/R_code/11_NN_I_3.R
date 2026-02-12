#==============================================================================#
# Exercise sheet 08: NN I - the basics                                         #
# Code template for exercise 03                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: load data ----------------------------------------------------------------
load("Data/NN_I_class_data.Rda")

# 02: data scaling -------------------------------------------------------------

# ranges
x1range <- range(x1)
x2range <- range(x2)
x1sc <- (x1 - x1range[1])/diff(x1range)
x2sc <- (x2 - x2range[1])/diff(x2range)

# We do not need to scale the outcome variable since it is categorical data
table(y)

# scale the validation data
# note that you have to scale the same transformations as for the training data 

x1valsc <- (x1val - ???)/???(x1range)
x2valsc <- (x2val - ???)/???(x2range)

# number of observations in training data
n <- length(x1sc)

# 03: helper functions ---------------------------------------------------------

# sigmoid function definition
sigmoid <- function(v) {1/(1 + exp(???))}

# plot the function, so you know what it looks like!
curve(???, -5, 5)

# Simple neural network
NN3 <- function(???, x1sc, x2sc) {
  
  # This function is the neural network
  # Input data is scaled x1 and x2
  # all weights are packed in one param vector 
  
  # unpack the param vector
  whl11 <- params[["whl11"]]
  whl12 <- params[["whl12"]]
  bhl1 <- params[["bhl1"]]
  wol1 <- params[["wol1"]]
  whl21 <- params[["whl21"]]
  whl22 <- params[["whl22"]]
  bhl2 <- params[["bhl2"]]
  wol11 <- params[["wol11"]]
  wol21 <- params[["wol21"]]
  wol31 <- params[["wol31"]]
  wol12 <- params[["wol12"]]
  wol22 <- params[["wol22"]]
  wol32 <- params[["wol32"]]
  bol1 <- params[["bol1"]]
  bol2 <- params[["bol2"]]
  bol3 <- params[["bol3"]]
  
  
  # hidden layer
  z1 <- whl11 * x1sc + whl12 * x2sc + bhl1
  z2 <- ???
  
  # activation
  a11 <- ???
  a12 <- ???
  
  # output layer
  a21 <- sigmoid(wol11 * a11 + wol12 * a12 + bol1)
  a22 <- ???
  a23 <- ???
  
  # matrix with fitted probabilities
  a_mat <- cbind(a21, a22, a23)
  
  # we have to normalize the activations to obtain probabilities adding up to 1
  # calculates the proportions for each row
  pi_mat <- apply(a_mat, 
                  MARGIN = 1,              # row-wise calculation
                  function(x) x/sum(x)     # function that normalizes each row
  ) 
  # we have to transpose `pi_mat` so that the format fits
  pi_mat <- t(pi_mat)
  return(pi_mat)
}  

# 04: parameter estimation -----------------------------------------------------

# To be able to run the whole process with one keyboard short (Ctrl + Enter),
# it is combined to one exprssion using `{...}`
{
  # number of iterations
  niter <- 10
  
  # initialise parameters (small non-zero and not equal)# 
  params <- c(whl11 = 0.010,
              whl12 = -0.011,
              bhl1 = 0.012,
              wol1 = -0.012,
              whl21 = 0.010,
              whl22 = -0.011,
              bhl2 = 0.012,
              wol11 = 0.017,
              wol21 = -0.009,
              wol31 = -0.012,
              wol12 = -0.008,
              wol22 = 0.011,
              wol32 = 0.013,
              bol1 = 0.01,
              bol2 = 0.02,
              bol3 = -0.1)
  
  # Initialisation of CE criteria
  bestCE <- Inf
  
  # further parameters
  n <- length(x1sc)
  window <- 1
  
  for(iter in ???){
    
    # We update parameter in turn and select the update if it gives a lower CE   
    Delta <- rep(0, length(params))  # initialise the change vector
    
    # define which parameter to perturb
    j <-  (iter %% length(params)) + 1
    
    # for later (part 6) reduce the window size incrementally
    # window <- 0.1 + 1/iter
    
    Delta[j] <- rnorm(1, 0, window) # generate the change in parameter and assign it
    
    # define the new parameter vector 
    params_perturbed <- params + Delta
    
    # call ??? for each observation
    fitted <- ???
    
    # loss function: check the definition of cross entropy to understand that
    # only the probabilities for the true outcome are required for Cross Entropy
    # calculation
    p_class_obs <- rep(NA, n)
    p_class_obs[y == 1] <- fitted[y == 1, 1]
    p_class_obs[y == 2] <- fitted[y == 2, ???]
    p_class_obs[y == ???] <- fitted[???]
    
    CE <- mean(-log(p_class_obs))
    
    # Improvement?
    # if CE is better, then update 
    if(CE < bestCE){
      params <- params_perturbed 
      bestCE <- CE
      best.fitted <- fitted
      
    }
    
  }
  cat(niter," iterations\n")
  cat ("Training CE:", round(bestCE, 5),"\n")
  
  stripchart((best.fitted[cbind(1:n, y)] ~ y), xlim=c(0,1))
  
  print(
    table(apply(best.fitted, MARGIN = 1, which.max),
          y, dnn = c("pred", "obs"))
  )
  
  val_predicted <- NN3(params, x1valsc, x2valsc)
  
  # cross entropy
  p_class_obs_val <- rep(NA, length(x1valsc))
  p_class_obs_val[yval == 1] <- ???
  p_class_obs_val[???] <- ???
  p_class_obs_val[???] <- ???
  
  valCE <- mean(???)
  cat ("Validation CE:", round(valCE,5), "\n")
  
}  # `Ctrl + Enter` here runs the whole {} block

# 05: More iterations ----------------------------------------------------------

# change the number of iterations in line 73 and repeat the main block

# The optimal parameters are
params

# 06: Incrementally reducing window width --------------------------------------

# turn the incrementally reducing window width on
# what do you have to change in the code given above?

# Do you get better results with the same amount of iterations? What would you
# have to do to examine this systematically?

# 07: Stability ----------------------------------------------------------------

# Re-run the code several times.
# Notice that the validation MSE value varies more than the training MSE.


# 07: final remark -------------------------------------------------------------

# Please don't hesitate to ask me about my idea of having altogether an 
# international data science Xmas party on December, 22nd at my place.
# My family and I would be more than happy to welcome you all!
# Idea: everyone contributes a little bit of food that is typical for her/his 
# home country and I provide the drinks.
# Looking forward to your comments, SW