
library(tidyverse)

# Comparing Central Tendency Measures -------------------------------------

#Create a new vector z with length 1000 as ezp(rnorm(n = 1000, mean = 0, sd = 0.1)),
#calculate the arithmetic mean, geometric mean, and median.

z <- exp(rnorm(n = 1000, mean = 0, sd = 1))
z; length(z)

#arithmetic mean
z_mean <- sum(z)/length(z)
z_mean
#check using mean function from r
mean(z)

#geometric mean
z_prod <- prod(z)
z_geomean <- z_prod^(1 / length(z))
#using log function
log_z <- log(z)
exp(mean(log_z))

#median
n1 <- sort(z)[length(z)/2]
n2 <- sort(z)[(length(z)/2)+1]
(z_median <- (n1+n2)/2)

#check
median(z)

#2 Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram()
z_tibble <- tibble(z=z)

z_hist <- z_tibble %>% 
  ggplot(aes(x=z)) +
  geom_histogram()
z_hist

#Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() .
z_hist +
  geom_vline(xintercept = z_mean, color="red")+
  geom_vline(xintercept = z_geomean, color="blue")+
  geom_vline(xintercept = z_median, color="green")

#Compare the values of the central tendency measures.


#Create a new vector z_rev as -z + maz(z) + 0.1, and repeat step 1 – 4.
z_rev <- -z +max(z) + 0.1

zrev_mean <- mean(z_rev)
zrev_geomean <- exp(mean(log(z_rev)))
zrev_median <- median(z_rev)

df_z_rev <- tibble(z_rev = z_rev)

rev_hist <- df_z_rev %>% 
  ggplot(aes(x=z_rev)) +
  geom_histogram()


rev_hist +
  geom_vline(xintercept = zrev_mean, color="red")+
  geom_vline(xintercept = zrev_geomean, color="blue")+
  geom_vline(xintercept = zrev_median, color="green")

#reading - tidyverse theme()

#Comparing Variation Measures
w <- rnorm(100, mean = 10, sd = 1)
head(w)
m<- w*1000
head(m)

#sd
m_sd <- sqrt(sum((m-mean(m))^2) / length(m))
m_sd
w_sd <- sqrt(sum((w-mean(w))^2) / length(w))
w_sd

sd(m)
sd(w)
      
#mad
w_mad <- median(abs(w - median(w)))
w_mad
m_mad <- median(abs(m - median(m)))
m_mad

#cv
(w_cv <- sd(w)/mean(w))
(m_cv <- sd(m)/mean(m))

w_sd/mean(w)
m_sd/mean(m)

#mad/mean
m_madmed <- m_mad/median(m) 
m_madmed
w_madmed <- w_mad/median(w) 
w_madmed


####END