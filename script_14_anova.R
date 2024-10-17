
# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

#ANOVA 

df_anova <- read_csv("data_raw/data_fish_length_anova.csv")
head(df_anova)
dim(df_anova)
distinct(df_anova, lake)

#anova using aov() function in r
m <- aov(formula = length ~ lake, data = df_anova)
print(m)
summary(m)

#distribution of data points 
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency

#Between Group comparison
## estimate overall mean
mu <- mean(df_anova$length)
mu
## estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group
print(df_g)

df_g <- df_g %>% 
  mutate(ss = dev_g * n) # or sum(df_g$dev_g)*50
print(df_g)

s_b <- sum(df_g$ss)
print(s_b)

#Within-group variability
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish
head(df_i)

# filter() & slice(): show first 3 rows each group
print(df_i %>% filter(lake == "a") %>% slice(1:3))
print(df_i %>% filter(lake == "b") %>% slice(1:3))
print(df_i %>% filter(lake == "c") %>% slice(1:3))

df_i_ss <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))
print(df_i_ss)

s_w <- sum(df_i_ss$ss)
print(s_w)

#Variability to Variance
# n_distinct() count the number of unique elements
var_b <- s_b / (n_distinct(df_anova$lake) - 1)
var_w <- s_w / (nrow(df_anova) - (n_distinct(df_anova$lake)))
print(var_b)
print(var_w)

#Test Statistic
#F-statistic #to estimate the extreme variance ratio in the probability distribution
f_value <- var_b / var_w
print(f_value)

#f-distribution - df() function
n_g <- n_distinct(df_anova$lake)
x <- seq(0, 10, by = 0.1)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "salmon") # observed F-statistic

# pf() estimate the probability of less than q
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)


#END