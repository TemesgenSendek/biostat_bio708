# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))


# poisson example --------------------------
# dpois(k, lambda)
dpois(3, lambda = 3.5)
# write the formula to confirm
(p <- (3.5^3 * exp(-3.5)) / factorial(3))

# change lambda from 0 to 10 by 0.1
lambda <- seq(0, 10, by = 0.1)
lambda
# probability
pr <- dpois(3, lambda = lambda)

# create a data frame
df_pr <- tibble(y = 3,
                lambda = lambda,
                pr = pr)
print(df_pr)

df_pr %>% 
ggplot(aes(y=pr, x=lambda)) +
  geom_point() +
  labs(y= "Pr(y=3)", x= "Lambda(mean of poisson dist)")

df_pr %>% 
  arrange(desc(pr))


# try lambda = 3 for y = 3, 2, 5
pr <- dpois(c(3, 2, 5), lambda = 3)
print(pr)

# probability of observing 3, 2, 5 simultaneously with lambda = 3
prod(pr)


# lambda = 0 - 10 by 0.01
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

# sapply repeats the task in FUN
# each element in "X" will be sequencially substituted in "z"
pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

# make a data frame and arrange by pr (likelihood)
df_pois <- tibble(lambda = lambda,
                  pr = pr)

# visualize
df_pois %>% 
  ggplot(aes(x = lambda, y = pr)) +
  geom_point() +
  labs(y = "Likelihood")+
  theme_bw()

df_pois %>% 
  arrange(desc(pr)) %>% 
  print()

mean(y)


# load garden plant data
df_count <- read_csv("data_raw/data_garden_count.csv")
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)

#END