# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))
#null hypothesis - considers the slope as zero,  
#lm trial
df_algae <- read_csv(here::here("data_raw/data_algae.csv"))
library(skimr)
skimr::skim(df_algae)
print(df_algae)
#scatter plot
df_algae %>% 
  ggplot(aes(x = conductivity, y = biomass)) +
  geom_point()

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity, data = df_algae)
summary(m)

##get estimates alpha, beta and error 
#residual is the distance of the points from the parallel y-axis
alpha <- coef(m)[1]
beta <- coef(m)[2]
df_algae %>% 
  ggplot(aes(x = conductivity, y = biomass)) +
  geom_point() + 
  geom_abline(intercept = alpha, slope = beta)


theta <- coef(m)
se <- sqrt(diag(vcov(m)))
se
t_value <- theta / se
t_value

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)


#get residual
#using function
resd_fun <- function(x, y, a, b){
  e <- y - (alpha + beta*x)
}
epsfun <- resd_fun(df_algae$conductivity, df_algae$biomass, alpha, beta)

alpha <- coef(m)[1]
beta <- coef(m)[2]
eps <- round(df_algae$biomass - (alpha + beta * df_algae$conductivity), 4)
eps0 <- round(resid(m), 4)
mean(eps)
ss <- sum(eps^2)
ss

#visualize error
# add error column
df_algae <- df_algae %>% mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity, y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha, slope = beta) + 
  geom_segment(aes(x = conductivity, xend = conductivity, 
                   y = biomass, yend = biomass - eps),
               linetype = "dashed")

# coefficient of determination
# null variance
ss_0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)
r2 <- 1 - ss / ss_0
print(r2)
summary(r2)

#END