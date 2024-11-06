# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

#COUNT DATA
df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)
dim(df_count)
#fit normal model
m_normal <- lm(count ~ nitrate, df_count)
summary(m_normal)


#visualize fit
b <- coef(m_normal)
df_count %>% ggplot(aes(x=nitrate, y=count))+
  geom_point() +
  geom_abline(intercept = b[1], slope = b[2])

#
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
summary(m_pois) 

# parameter estimates and their SEs
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se #z-value= estimate/SE
print(z_value)

g_normal <- df_count %>% 
  ggplot(aes(x=nitrate, y=count))+
  geom_point() +
  geom_abline(intercept = b[1], slope = b[2])

# estimate Pr(>|z|) using a standardized normal distribution
p_value <- (1 - pnorm(abs(z_value), mean = 0, sd = 1)) * 2
print(p_value)

#prediction
df_pred_pois <- tibble(nitrate = seq(min(df_count$nitrate), 
                                max(df_count$nitrate),
                                length=100),
                  y_pois=exp(theta[1] +theta[2]*nitrate))

g_normal + 
  geom_line(data = df_pred_pois,
            aes(y = y_pois),
            color = "salmon")


#offset term
df_count_ue <- df_count %>% 
  mutate(area=rpois(nrow(.), 10),
         count_ue=count*area)

df_count_ue %>% ggplot(aes(area, count_ue)) +
  geom_point()


#glm with offset term
m_pois_ue <- glm(count ~ nitrate + offset(log(area)),
                 data = df_count_ue,
                 family = "poisson")
summary(m_pois_ue)


# proportional data -------------------------------------------------------

# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x, y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",x = "logit(x)")

df_mussel <- read_csv("data_raw/data_mussel.csv")
print(df_mussel)

df_mussel <- df_mussel %>% mutate(prop_fert = n_fertilized / n_examined)


df_mussel %>% 
  ggplot(aes(x=density, y=prop_fert)) +
  geom_point() +
  labs(x="Mussel density", y="Proportion of eggs fertilized")

#GLM with binomial error distribution 
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, 
                     data = df_mussel,
                     family="binomial")
m_binom

# make prediction
df_pred <- df_mussel %>% 
  reframe(density=seq(min(density),
                      max(density),
                      length=100))
  
  
# y_binom is inv.logit-transformed because predict() returns values in logit-scale
y_binom <- predict(m_binom, newdata = df_pred) %>% 
  boot::inv.logit()

df_pred <- df_pred %>% 
  mutate(y_binom)

# manual conversion 
#my.inv.logit <- function (x){
#  exp(x) / 1 + exp(x)
#}
#my_y<- predict(m_binom, newdata = df_pred) %>% 
#  my.inv.logit

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line(aes(y = y_binom), data = df_pred) +
  labs(x = "density", y = "prop of eggs fertilized")

# binomial distribution with variable number oif trials
df_mus_var <- df_mussel %>% mutate(n_examined = rpois(nrow(.), lambda = 40))

m_binom_var <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, 
               data = df_mus_var,
               family="binomial")
m_binom_var



#END 