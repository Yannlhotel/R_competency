#==============================================================================#
# Exercise sheet 09: NN II - 1-dim optimization                                #
# Code template for exercise 03                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: load data ----------------------------------------------------------------
# no data required

# 02: Demo: lean back mode------------------------------------------------------

# Function and first derivative 
f_poly <- function(x) 3 * x^4 + 5 * x^3 - 20 * x^2 + 8 * x^1 + 10
f_poly_deriv <- function(x) 12 * x^3 + 15 * x^2 - 40 * x^1 + 8

# Function graph
xlim <- c(-4.2, 3.2)
curve(f_poly, from = xlim[1], to = xlim[2])

# Finding "a" minimum: initial conditions
x <- -4    # starting at x = -4
s <- 0.01  # step size 
points(x, f_poly(x), pch = 16, col = "Blue")

# Calculating the "gradient": -f'(x)
f_poly_deriv(x)         # gradient 
-f_poly_deriv(x) * s    # moving in direction of descent

# visualisation
abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Blue", lty = 2)

# new positions and visualisation
x <-  x - f_poly_deriv(x) * s
points(x, f_poly(x), pch = 16)
abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Blue", lty = 2)

# new positions and visualisation
x <-  x - f_poly_deriv(x) * s
points(x, f_poly(x), pch = 16)
abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Blue", lty = 2)

# new positions and visualisation
x <-  x - f_poly_deriv(x) * s
points(x, f_poly(x), pch = 16)
abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Blue", lty = 2)

# a loop instead of all the single steps
for(i in 1:10){
  x <-  x - f_poly_deriv(x) * s
  points(x, f_poly(x), pch = 16)
  abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Blue", lty = 2)
}

# Final results
cat("minimum at x=", x)
cat("function slope at minimum at f'(x)=", f_poly_deriv(x))

# try the starting point x = 0.5 
x <-  0.5

# a loop instead of all the single steps
for(i in 1:10){
  x <-  x - f_poly_deriv(x) * s
  points(x, f_poly(x), pch = 16, col = "red")
  abline(a = f_poly(x) - x * f_poly_deriv(x), b = f_poly_deriv(x), col = "Red", lty = 2)
}

# Final results
cat("minimum at x=", x)
cat("function slope at minimum at f'(x)=", f_poly_deriv(x))

# Is this the minimum?
# - if yes: why?
# - if no: what should you do?

# 03: The sine -----------------------------------------------------------------

# Repeat the above exercise using
f_sin <- function(x) sin(x)
f_sin_deriv <- function(x) ???

# Investigate the range 
xlim <- c(-pi, 3 * pi)
curve(f_sin, from = xlim[1], to = xlim[2], ylim = c(-1.5, 1.5))

# a loop instead of all the single steps
s <- 0.01
x <- 1.2  # try 1.5, 1.7, 7.8
points(x, f_sin(x), pch = 16, col = "red")
for(i in 1:10){
  
  # increase slowly the step size
  s <- s * ???
  x <-  x - f_sin_deriv(x) * s
  
  points(x, f_sin(x), pch = 16)
  abline(a = f_sin(x) - x * f_sin_deriv(x), b = f_sin_deriv(x), col = "Blue", lty = 2)
}

# Final results
cat("minimum at x=", x)
cat("function slope at minimum at f'(x)=", f_sin_deriv(x))


# 03: Another function ---------------------------------------------------------

# Repeat the above exercise using
f_expSq <- function(x) exp(x^2 + 2*x - 4)
f_expSq_deriv <- ???

# Investigate the range 
xlim <- c(-4, 2)
curve(f_expSq, from = xlim[1], to = xlim[2])

# apply the code above to investigate the function f_expSq

???
