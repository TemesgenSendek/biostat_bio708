# setup
rm(list = ls())

# load package
source(here::here("set_library.R"))
#Data set
df_h0 <- read_csv("data_raw/data_plant_height.csv")

# exercise 1 --------------------------------------------------------------
#Obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms of sample means and unbiased variances (use var()).

#50 measures
mu50_i <- NULL
var50_i <- NULL
var50_ub_i<-NULL
for (i in 1:100) {
  df50_i <- df_h0 %>% sample_n(50)
  mu50_i[i] <- mean(df50_i$height)
  var50_i[i] <- sum((df50_i$height - mean(df50_i$height))^2) / nrow(df50_i)
  var50_ub_i[i] <- var(df50_i$height)
}
#true mean
mu50<- mean(df50_i$height)


#100 measures
mu100_i <- NULL
var100_i <- NULL
var100_ub_i<-NULL
for (i in 1:100) {
  df100_i <- df_h0 %>% sample_n(100)
  mu100_i[i] <- mean(df100_i$height)
  var100_i[i] <- sum((df100_i$height - mean(df100_i$height))^2) / nrow(df100_i)
  var100_ub_i[i] <- var(df100_i$height)
}

#true mean and var
mu100<- mean(df100_i$height)
var100 <- sum((df100_i$height - mean(df100_i$height))^2) / nrow(df100_i)
mu50<- mean(df50_i$height)
var50 <- sum((df50_i$height - mean(df50_i$height))^2) / nrow(df50_i)

#ggplot histogram for 50 ---------------
df_sample <- tibble(mean = mu50_i, variance = var50_ub_i)
# histogram for mean
g_mu50 <- df_sample %>% 
  ggplot(aes(x = mean)) +
  geom_histogram()

# histogram for variance
g_var50 <- df_sample %>% 
  ggplot(aes(x = variance)) +
  geom_histogram() 

g_mu50 / g_var50

#ggplot histogram for 100 ---------------
df_sample <- tibble(mean = mu100_i, variance = var100_ub_i)

# histogram for mean
g_mu100 <- df_sample %>% 
  ggplot(aes(x = mean)) +
  geom_histogram() 

# histogram for variance
g_var100 <- df_sample %>% 
  ggplot(aes(x = variance)) +
  geom_histogram() 

g_mu100/g_var100



# exercise 2 --------------------------------------------------------------

df_nonrand <- lapply(X=c(50, 100),
                     function(x) {
                       mean <- var <- NULL
                       
                       for(i in 1:100) {
                         df_h10 <- df_h0 %>% filter(height >= 10)
                         dfh10_i <- df_h10 %>% sample_n(x)
                         mean[i] <- mean(dfh10_i$height)
                         var[i]<- var(dfh10_i$height)
                       }
                       cout <- tibble(n=x, mean = mean, variance = var)
                       return(cout)
                  }) %>% 
  bind_rows()

df_nonrand %>% ggplot(aes(x=mean, colour = factor(n))) +
  geom_density()
df_nonrand %>% ggplot(aes(x=variance, colour = factor(n))) +
  geom_density()



#END