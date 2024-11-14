# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))


#1 Binomial distribution ----------------------------

y=c(2,2,0,0,3,1,3,3,4,3)
N=10
lambda <- seq(0, 1, by = 0.01)

pr <- sapply(X = lambda,
             FUN = function(z) prod(dbinom(y, size = N, prob = z)))

df_binom <- tibble(lambda = lambda, pr = pr) 

df_binom %>% 
  arrange(desc(pr)) %>% 
  print()

#From the set of p examined, find the parameter value that maximizes the likelihood.
0.21 

#calculate N×p and compare with the mean
N*0.21
mean(y)

#2 Normal distribution ----------------------------

