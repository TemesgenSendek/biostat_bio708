# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

#ANOVA LAB

#Exercise 1-----------------------
#Application to PlantGrowth
data(PlantGrowth) 
PlantGrowth <- PlantGrowth %>% tibble()
head(PlantGrowth)
#two columns: weight, group. Create figures similar to 
#???Figure 4.1 in Chapter 4.

PlantGrowth %>% 
  ggplot(aes(x = group, y = weight)) +
  geom_violin(alpha = 0.2, draw_quantiles = 0.5) +
  geom_jitter(alpha = 0.2) 


#Conduct an ANOVA
pg_aov <- aov(formula = weight ~ group, data = PlantGrowth)
summary(pg_aov)

#Discuss what values to be reported in a scientific article?

##Generally th eproduced summary should be reported in a scientific article.
##sum of squares
##F value
##probability for accepting or rejecting the null hypothesis



#Exercise 2 -----------------------
#Calculate the F-value manually using the plantGrowth data
mu <- mean(PlantGrowth$weight)
s_b <- PlantGrowth %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), 
            dev_g = (mu_g - mu)^2,
            n = n(),
            ssq = dev_g *n) %>% 
  pull(ssq) %>% 
  sum()
print(s_b)

var_b <- s_b / (n_distinct(PlantGrowth$group) - 1)

#Sum of Squares
s_w <- PlantGrowth %>% 
  group_by(group) %>% 
  mutate(mu_g = mean(weight)) %>% 
  ungroup() %>% 
  mutate(dev_i = (weight - mu_g)^2) %>% 
  summarize(ssq = sum(dev_i))%>% 
  pull(ssq) %>% 
  sum()

var_w <- s_w / (nrow(PlantGrowth) - (n_distinct(PlantGrowth$group)))

#F-statistic
f_value <- var_b / var_w
print(f_value)


#create a function to calculate F value
aov_fun <- function(data) {
  s_b <- data %>% 
    group_by(group) %>% 
    summarize(mu_g = mean(weight),
              dev_g = (mu_g - mu)^2, 
              n = n(),
              ssq = dev_g *n) %>% 
    pull(ssq) %>% 
    sum()
  var_b <- s_b / (n_distinct(data$group) - 1)
  
  s_w <- data %>% 
    group_by(group) %>% 
    mutate(mu_g = mean(weight)) %>%
    ungroup() %>% 
    mutate(dev_i = (weight - mu_g)^2) %>% 
    summarize(ssq = sum(dev_i)) %>% 
    pull(ssq) %>% 
    sum()
  var_w <- s_w / (nrow(data) - (n_distinct(data$group)))
  
  f_value <- var_b / var_w
  return(f_value)
}

aov_fun(PlantGrowth)


#END -----------------------------------------------------------=