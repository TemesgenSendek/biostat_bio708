# setup
rm(list = ls())

# load package
source("code/set_library.R")

df_fl <- read_csv("data_raw/data_fish_length.csv")
print(df_fl)

# unique returns unique values as a vector
unique(df_fl$lake)
# distinct returns unique values as a tibble
distinct(df_fl, lake)

# group mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake, xend = lake,
                   y = mu_l - sd_l, yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake, y = mu_l),
             size = 3) +
  labs(x = "Lake", y = "Fish body length")



#Test Statistic
## take another look at df_fl_mu
print(df_fl_mu)

## pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
  pull(mu_l)

## lake a
print(v_mu[1])
# lake b
print(v_mu[2])
# difference
v_mu[1] - v_mu[2]


# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

print(df_t)

# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)





#









#t-test
x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)