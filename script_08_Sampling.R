# setup
rm(list = ls())

# load package
source("code/set_library.R")

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10,
                height = h,
                unit = "cm") %>% 
  mutate(mean_h = mean(h),
         var_h = sum((h-mean_h)^2)/nrow(.)) # dor refer a data frame
df_h1

# samples from same population
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)
df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))
print(df_h2)


# Sampling ----------------------------------------------
# load csv data on R
#include 1000 individuals
(df_h0 <- read_csv("data_raw/data_plant_height.csv"))

#true mean and true variance
mu<- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)

#random sampling
(df_i <- df_h0 %>% 
  sample_n(10))

# getting 100 sets of 10 samples
##estimate mean for each 10
mu_i <- NULL
var_i <- NULL
for (i in 1:100) {
  df_i <- df_h0 %>% sample_n(10)
  mu_i[i] <- mean(df_i$height)
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}


#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var

#unbiased estimate
mu_i <- var_i <- var_ub_i <- NULL 

for (i in 1:100) {
  df_i <- df_h0 %>% sample_n(size = 10) 
  mu_i[i] <- mean(df_i$height)
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  var_ub_i[i] <- var(df_i$height)
}

# histogram for the unbiased 
g_ubi <- df_sample %>% 
  ggplot(aes(x = var_ub_i)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_mu / g_var / g_ubi


#END 
