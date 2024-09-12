# Descriptive Statistics --------------------------------------------------
library(tidyverse)

# Central Tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)
x
y
#Arithmetic Mean --------------------------------------------------------
# for vector x
n_x <- length(x) # the length  x
sum_x <- sum(x) # summation for x
mu_x <- sum_x / n_x # arithmetic mean
print(mu_x)

#using function
arth_mean <- function(y){
  n_y <- length(y)
  sum_y <- sum(y)
  mu_y <- sum_y / n_y
  print(mu_y)
}
arth_mean(y)

#using mean function
mean(x)
mean(y)


#Geometric Mean --------------------------------------------------------

# for vector x
#prod() multiply all elements
prod_x <- prod(x)
prod_x^(1 / length(x)) # ^ means power

log_x <- log(x)
exp(mean(log_x))

# for vector y
mug_y <- prod(y)^(1 / length(y))
print(mug_y)

log_y <- log(y)
exp(mean(log_y))
  
  
#median --------------------------------------------------------
median(x)

#for y
index <- (length(y)+1)/2
sort(y)[index]



# Variation ---------------------------------------------------------------

##calculate variance
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

varx <- sum((x-mean(x))^2)/length(x)
varx
sdx <- sqrt(varx)
sdx

(vary <- sum((y-mean(y))^2)/length(y))
(sdy <- sqrt(vary))

# Quantile ----------------------------------------------------------------
#quantile()
quantile(x)
x25 <- quantile(x, 0.25)
x25
x75 <- quantile(x, 0.75)
x75
(iqr_x <- abs(x25-x75))

y_q <- quantile(y, c(0.25, 0.75))
y_q

(iqr_y <- abs(y_q[1] - y_q[2]))



# Coefficient of variation ------------------------------------------------
#cv for x and y
(cv_x <- sdx/mean(x))
(cv_y <- sdx/mean(y))

abs(diff(quantile(x, c(0.25, 0.75)))/median(x))
abs(diff(quantile(y, c(0.25, 0.75)))/median(y))




###END
