# setup
rm(list = ls())

# load package
source(here::here("set_library.R"))
#Data set
df_h0 <- read_csv("data_raw/data_plant_height.csv")

#class work
## histogram
df_h0 %>% 
  ggplot(aes(x=height)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(height)))

#probability density function - dnorm()
# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), 
         max(df_h0$height), 
         length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density", x="Height") # re-label

#probability less than x<10
p10 <- pnorm(q=10, mean = mu, sd=sigma)
p10
p5 <- pnorm(q=5, mean = mu, sd=sigma)
p5
p10-p5

p20 <- pnorm(q=20, mean=mu, sd=sigma)
p20
p10_20 <- p20-p10

#create histogram with estimates
(xmin <- floor(min(df_h0$height)))
(xmax <- ceiling(max(df_h0$height)))
bin <- seq(xmin, xmax, by=1)
bin

p <- NULL
for (i in 1:(length(bin) - 1)){
  p[i] <- pnorm(bin[i+1], mean=mu, sd=sigma) - pnorm(bin[i], mean = mu, sd=sigma)
}
p

df_prob <- tibble(p, bin=bin[-length(bin)]+0.5) %>% 
  mutate(freq=p * nrow(df_h0))

df_h0 %>% 
  ggplot(aes(x=height)) +
  geom_histogram(binwidth = 1, center=0.5) +
  geom_point(data = df_prob, 
             aes(x=bin, y=freq), color="salmon") +
  geom_line(data = df_prob, 
            aes(x=bin, y=freq), color="salmon")


#Discrete Variable
##Probability Mass Function
df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, center = 0) 


# create a vector of 0 - 10, integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

df_prob <- tibble(y=pm, x=x) %>% 
  mutate(freq=y*nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, center = 0) +
  geom_line(data = df_prob, 
            aes(x=x, y=freq),linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x=x,y=freq))


#END
