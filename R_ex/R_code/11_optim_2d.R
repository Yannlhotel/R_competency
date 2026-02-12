#==============================================================================#
# Exercise sheet 09: NN II - 2-dim optimization                                #
# Code template for exercise 03                                                #
#==============================================================================#
rm(list = ls(all.names = TRUE))


# 00: packages -----------------------------------------------------------------
# no additional packages required

# 01: load data ----------------------------------------------------------------
# no data required

# 02: lean back mode: watch the optimization -----------------------------------

# a 2-dim function in 2 dimensions
f2d <- function(x1, x2) (x1^4 * x2^2 + (x1 - 1)^2 * (x2 - 1)^4)
# define the partial derivatives 
dfdx1 <- function(x1, x2) (4*x1^3*x2^2+2*(x1-1)*(x2-1)^4)
dfdx2 <- function(x1, x2) (2*x1^4*x2+4*(x1-1)^2*(x2-1)^3)

#create a contour plot of f2d
x1axis <- seq(-2, 1.5, length = 100)
x2axis <- seq(-1.5, 1.5, length = 100)
z <- matrix(NA, 100, 100)
for(i in 1:100)  for(j in 1:100) z[i,j] <- (f2d(x1axis[i], x2axis[j]))
contour(x1axis, x2axis, z, levels = 2^(-5:11)*0.02, asp = 1)


# starting point (-1,1)
x1 <- -1
x2 <- 1
f2d(x1, x2)
dfdx1(x1, x2)
dfdx2(x1, x2)

points(x1, x2, pch = 16)
s <- 0.1
# the gradient
arrows(x1, x2, x1 + s * dfdx1(x1, x2), x2 + s * dfdx2(x1, x2), col = 2, length = 0.1)
# the negative gradient
arrows(x1, x2, x1 - s * dfdx1(x1, x2), x2 - s * dfdx2(x1, x2), col = 4, length = 0.1)

# New starting point
x1_new <- x1 - s * dfdx1(x1, x2)
x2_new <- x2 - s * dfdx2(x1, x2)

points(x1_new, x2_new, pch = 16)

for(i in 1:10){
  x1_new <- x1 - s*dfdx1(x1, x2)
  x2_new <- x2 - s*dfdx2(x1, x2)
  x1 <- x1_new
  x2 <- x2_new
  points(x1,  x2,  pch = 16)
}

# Without adapting step length
contour(x1axis, x2axis, z, levels = 2^(-5:11)*0.02, asp = 1)
x1 <- -1
x2 <- 1
for(i in 1:100){
  x1_new <- x1-s*dfdx1(x1, x2)
  x2_new <- x2-s*dfdx2(x1, x2)
  x1 <- x1_new
  x2 <- x2_new
  points(x1, x2, pch = 16)
}

#Slowly increasing step length
contour(x1axis, x2axis, z, levels = 2^(-5:11)*0.02, asp = 1)
x1 <- -1
x2 <- 1
s <- 0.1
for(i in 1:100){
  x1_new <- x1 - s * dfdx1(x1, x2)
  x2_new <- x2 - s * dfdx2(x1, x2)
  x1 <- x1_new
  x2 <- x2_new
  points(x1, x2, pch = 16)
  s  <-  s * 1.1
}


# another starting point (0.6,-0.6)
contour(x1axis,x2axis,z,levels = 2^(-5:11)*0.02,asp = 1)
x1 <- 0.6
x2 <- -0.6
s <- 0.1
for(i in 1:100){
  x1_new <- x1 - s * dfdx1(x1, x2)
  x2_new <- x2 - s * dfdx2(x1, x2)
  x1 <- x1_new
  x2 <- x2_new
  points(x1, x2, pch = 16)
  s  <-  s * 1.1
}


# 03: exercise 2.2b) -----------------------------------------------------------
# function and partal derivatives
f2d <- function(x1,x2) x1^2+x2^2+2*x1-4*x2-1
dfdx1 <- function(x1,x2) ???
dfdx2 <- function(x1,x2) ???

# visualisation
x1axis <- seq(-4,2.5,length=100)
x2axis <- seq(-1,4,length=100)
z <- matrix(NA,100,100)
for(i in 1:100) for(j in 1:100) z[i,j] <- (f2d(x1axis[i],x2axis[j]))
contour(x1axis,x2axis,z,asp=1,levels=c(-5.5,-5:10))

# starting point (0, 0)
x1 <- 0
x2 <- 0
points(x1, x2, pch = 16)
s <- 0.1

# the gradient
arrows(x1, x2, x1 + s * ???, x2 + s * ???, col = 2, length = 0.1)

# the negative gradient
arrows(x1, x2, x1 ???, x2 ???, col = 4, length = 0.1)

# New starting point
x1_new <- x1 ???
x2_new <- x2 ???

points(x1_new, x2_new, pch = 16)

# Slowly increasing step length
s <- 0.1
for(i in 1:10){
  x1_new <- ???
  x2_new <- ???
  x1 <- x1_new
  x2 <- x2_new
  points(x1, x2, pch = 16)
  s  <-  s * 1.1
}

# Final results
cat("minimum at x=(", x1, ", ", x2, ")")
cat("gradient at minimum at -(f_x1'(x), f_x2'(x))=(", dfdx1(x1, x2), ", ", dfdx2(x1, x2), ")")

