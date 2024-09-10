#ggplot - Sep 3, 2024
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

#Visualization -----------
##Point -------
#geom_point() : Add a point layer

# basic plot without pipe
ggplot(data=iris, aes(x = Sepal.Length,y = Sepal.Width)) +
geom_point()

#with pipe
iris %>% 
  ggplot(aes(x = Sepal.Length,y = Sepal.Width, color=Species)) +
  geom_point()

#Line ----------
#geom_line() : Add a line layer

# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(lwd=2, color="blue")

#Histogram ---------------
#geom_histogram() : add a histogram layer

# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

# change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

# change bin number
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)

# basic plot; coloring the hist
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(col="blue", fill="green")

# Boxplot --------
#geom_boxplot() : add a boxplot layer

# basic plot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             color = Species)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

# change fill transparency by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(alpha=0.25)

# Violin Chart -----
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_violin(width=1, size=0.2, alpha=0.6) +
  geom_boxplot(color="black", alpha=0.1, width=0.1)
  

# Exercise - R_Graph gallery -----------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(forcats)
library(hrbrthemes)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
head(data)
data <- data %>% 
  gather(key="text", value="value") %>% # to make the format longer
  mutate(text = gsub("\\.", " ",text)) %>% #to replace "." by space 
  mutate(value = round(as.numeric(value),0)) %>% 
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", 
                     "Little Chance", "Chances Are Slight", "Almost No Chance"))
head(data)
tail(data)

## Ridgeline chart --------
data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot(aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)) +
  xlab("") +
  ylab("Assigned Probability (%)")


## Violin Chart-----
data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")



# END 