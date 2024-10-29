# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

# t-test equivalence with lm ---------------------------

df_fl <- read_csv("data_raw/data_fish_length.csv")
print(df_fl)

# group means
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

# mu_a: should be identical to intercept
v_mu
v_mu[1]

v_mu[2] - v_mu[1]

m <- lm(data = df_fl, length ~ lake)
summary(m)

#look into details of t-test
lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = lake_b, y = lake_a, var.equal = T)


# Multiple-Group Case - a, b, c
#anova equivalence with lm ---------------------------------------------
df_anova <- read_csv("data_raw/data_fish_length_anova.csv")
print(df_anova)

# group means
v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1], # mu_a: should be identical to intercept
        v_mu[2] - v_mu[1], # mu_b - mu_a: should be identical to the slope for lakeb
        v_mu[3] - v_mu[1])) # mu_c - mu_a: should be identical to the slope for lakec


lm(length ~ lake, data=df_anova) %>% summary()

aov(data=df_anova, length ~ lake) %>% summary()



# Combine Multiple Types of Variables

iris <- as_tibble(iris)
print(iris)
distinct(iris, Species)

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data=iris) 
summary(m_iris) # in the result, setosa act as baseline




# create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

# make prediction based on supplied values of explanatory variables
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)


iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) # redefine y values for lines; x and color are inherited from ggplot()


#END 