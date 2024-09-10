#load library
library(tidyverse)

#load data
iris <- as_tibble(iris)


# Function ----------------------------------------------------------------
##function() to create function
set.seed(1)
x <- rnorm(100, mean = 10, sd=100)
x
hist(x)
mean(x)

#without function
sig <- sd(x)
mu <- mean(x)
cv <- mu/sig
cv

#with function
fun_cv <- function(x) {
  cv <- mean(x)/sd(x)
  return(cv)
}
fun_cv(x)

# standardization
x0 <- x- mean(x) # centering
z0 <- x0-sd(x) #scaling
mean(x0)
sd(x0)

fun_scl <- function(x) {
  z <- (x - mean(x))/ sd(x)
  return(z)
}

fun_scl(x)
mean(fun_scl(x))
sd(fun_scl(x))


f0 <- function(phi, zeta) {
  cout <- 2*phi + rnorm(1) *zeta
  return(cout)
}

f0(phi = 2, zeta = 3)



# Apply family ------------------------------------------------------------
set.seed(1)
mat <- matrix(rnorm(25), nrow = 5, ncol=5)

## apply() -----------------------
apply(mat, MARGIN = 1, FUN = mean)
apply(mat, MARGIN = 2, FUN = mean)
apply(mat, MARGIN = 1, FUN = fun_cv) #for a function you defined

#for dataframe
apply(iris %>% select(1:4),
      MARGIN = 2, FUN=mean)


## sapply() ------------------------------------------------------------------
#sapply() - for list
x<-rnorm(10); length(x)
y<-rnorm(100); length(y)
z<-rnorm(5); length(z)
ls_xyz <- list(x,y,z)

sapply(ls_xyz, mean)

## lapply() -----------------------------------------------------------------
x<- rpois(10, lambda=5)
y<- rpois(100, lambda = 5)
z<-rep(letters[1:3], 10)
ls_xyz<-list(x,y,z)
ls_xyz

#remove duplicates and get the unique elements
unique(x)
unique(y)
unique(z)
lapply(ls_xyz, FUN=unique)

# try to get the first element from each vector

fun.frst <- function(x) {
  return(x[1])
}

lapply(ls_xyz, FUN = fun.frst)
sapply(ls_xyz, FUN = fun.frst)

#2nd way 

lapply(ls_xyz, 
       FUN=function(x)
         {return(x[1])})


# Looping -----------------------------------------------------------------
#for loop is to repeat a work inside {}
x<-seq(0, 10, by=0.25)

y<- NULL
for(i in 1:10) {
  y[i] <- 2 * x[i]
}
y

y<- NULL
for(i in 1:10) {
  y <- 2 * x[i]
}
y


# END
