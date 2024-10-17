# setup
rm(list = ls())

# load package
source(here::here("set_library.R"))

#Excersise
df10a <- rnorm(10, mean = 10, sd=5)
df10b <- rnorm(10, mean = 12, sd=5)
df100a <- rnorm(100, mean = 10, sd=5)
df100b <- rnorm(100, mean = 12, sd=5)

t.test(df10a, df100a, var.equal = TRUE)
t.test(df10b, df100b, var.equal = TRUE)
t.test(df10a, df10b, var.equal = TRUE)
t.test(df100a, df100b, var.equal = TRUE)


#Difference and Uncertainty
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)
b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

length(a1);length(a2);length(b1);length(b2)
df <- tibble(values = c(a1, a2, b1, b2),
             group=rep(c("a1","a2", "b1", "b2"), each=10))
df

df_mnsd <- df %>% 
  group_by(group) %>% 
  summarize(mean = mean(values), 
            sd = sd(values),
            n = n()) 

df_mnsd

df %>% 
  ggplot(aes(x = group, y = values)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.25) + 
  geom_segment(data = df_mnsd,
               aes(x = group, xend = group,
                   y = mean - sd, yend = mean + sd)) +
  geom_point(data = df_mnsd,
             aes(x = group, y = mean),
             size = 3) 

df_mnsd
t.test(a1,a2, var.equal = F)
t.test(b1,b2, var.equal = F) 
t.test(a1,b1, var.equal = F) 
t.test(a2,b2, var.equal = F) 


#Bonus Exercise: Welch’s t-test
#Reproduce the results of t.test(x, y, var.equal = FALSE) 
#without using this function (report t-statistic, degrees of freedom, and p-value).

# pull values as a vector
df_mnvar <- df %>% 
  group_by(group) %>% 
  summarize(mean = mean(values), 
            var = var(values))

dfa12 <- df_mnvar[1:2,]
dfb12 <- df_mnvar[3:4,]

# pull values as a vector
v_mu <- pull(dfa12, mean)
v_var <- pull(dfa12, var)
v_n <- pull(dfa12, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]
var_p

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))
t_value

#the t-values from both results are equal
t.test(a1,a2, var.equal = F)


#End