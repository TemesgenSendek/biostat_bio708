# setup
rm(list = ls())

# load package
source(here::here("set_library.R"))
#Data set
#Generate a variable with 50 observations.
set.seed(1)
df50 <- rnorm(50, mean = 100, sd=15)
df50 <- tibble(variable = df50) 
head(df50)
df50 %>%
  ggplot(aes(variable)) +
  geom_histogram()+
  geom_vline(aes(xintercept = mean(variable)))


#probability density
x50 <- seq(min(df50$variable), 
         max(df50$variable), 
         length = 50)
mean <- mean(df50$variable)
sigma <- sd(df50$variable)

pd50 <- dnorm(x50, mean = mean, sd = sigma)

tibble(x=x50, y=pd50) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_line() +
  labs(x = "variable", y="pd" )


(xmin <- floor(min(df50$variable)))
(xmax <- ceiling(max(df50$variable)))
bin <- seq(xmin, xmax, by=1)
bin

p <- NULL
for (i in 1:(length(bin) - 1)){
  p[i] <- pnorm(bin[i+1], mean=mean, sd=sigma) - pnorm(bin[i], mean = mean, sd=sigma)
}
p

df50_prob <- tibble(p, bin=bin[-length(bin)]+0.5) %>% 
  mutate(freq=p * nrow(df50))

df50 %>% 
  ggplot(aes(x=variable)) +
  geom_histogram(binwidth = 1, center=0.5) +
  geom_point(data = df50_prob, 
             aes(x=bin, y=freq), color="salmon") +
  geom_line(data = df50_prob, 
            aes(x=bin, y=freq), color="salmon")

# ======================================================
#Poisson Distribution
##Generate a variable with 1000 observations
lambda <- 20
x2 <- rpois(1000, lambda=lambda)
x2min <- min(x2)
x2max <- max(x2)

bin2 <- seq(x2min, x2max, by=1)
p <- dpois(bin2, lambda=lambda)

df2_prob <- tibble(x=bin2) %>% 
  mutate(p=dpois(bin2, lambda=lambda),
         freq=p*length(x2))

df2_prob %>% ggplot(aes(x=x, y=freq))+
  geom_line(data = df2_prob, aes(x=x, y=freq))+
  geom_line(linetype = "dashed")+
  geom_point()
  
tibble(x=x2) %>% 
  ggplot(aes(x=x)) +
  geom_histogram(fill="darkblue")+
  geom_line(data = df2_prob, aes(x=x, y=freq), color="red")+
  geom_point(data = df2_prob, aes(x=x, y=freq), color="red")


#END 
