# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))
#Develop regression models
#R provides a built-in data set called iris. 
#The iris data contain data points from three different species (Species column). 
#Split this data set by species (create three separate data frames) and 
#perform regression for each species separately to analyze the relationship 
#between Sepal.Width (y, response variable) and Petal.Width (x, explanatory variable).

head(iris)
levels(iris$Species)
setosa <- iris %>% filter(Species == "setosa")
versicolor <- iris %>% filter(Species == "versicolor")
virginica <- iris %>% filter(Species == "virginica")

reg_set <- lm(Sepal.Width ~ Petal.Width, data = setosa)
summary(reg_set)
reg_vers <- lm(Sepal.Width ~ Petal.Width, data = versicolor)
summary(reg_vers)
reg_virg <- lm(Sepal.Width ~ Petal.Width, data = virginica)
summary(reg_virg)

#without explanatory variable
m0 <- lm(Sepal.Width ~ 1, data=iris)
summary(m0)

#Multiple explanatory variables
#Regression analysis can involve multiple explanatory variables. 
#To explore this, consider utilizing Petal.Length as an 
#additional explanatory variable for each species. 
#Then, investigate 
#(1) the variations in estimates of regression coefficients and 
#(2) the differences in the coefficients of determination 
#compared to the model with only a single explanatory variable.


mw <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data=iris)
summary(mw)

mw <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Petal.Width:Petal.Length, data=iris)
summary(mw)


# claculate coffiecient of determination
ss <- sum(resid(lm(data = setosa, Sepal.Width ~ Petal.Width))^2)
seto <- sum((setosa$Sepal.Width - mean(setosa$Sepal.Width))^2)
r2 <- 1 - ss / seto
print(r2)


# for loop
sp <- unique(iris$Species)
n_sp <- n_distinct(iris$Species)
list_m <- NULL
for(i in 1:n_sp){
  list_m[[i]] <- lm(Sepal.Width ~ Petal.Width, 
                  data = iris %>% filter(Species == sp[i]))
   }



#END
