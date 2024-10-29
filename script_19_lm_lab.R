# Set up ------------------------------------------------------------------
rm(list = ls())
source(here::here("set_library.R"))

data("iris")
m_iris <- lm(Petal.Length ~ Petal.Width + Species, data=iris)

#Normality Assumption applied on residuals
#Shapiro-Wilk test to the model m_iris
shapiro.test(m_iris$residuals)


#Model Interpretation
#The model depicted in Figure 7.1 can be interpreted as follows: 
#  “each species has a distinct intercept value.” 
#Extract the intercept values for each species from the m_iris object.
m_iris$coefficients

#Setosa intercept
m_iris$coefficients[1]
#versicolor intercept
m_iris$coefficients[1] + m_iris$coefficients[3]
#virginica intercept
m_iris$coefficients[1] + m_iris$coefficients[4]


#Alternative Model
m2_iris <- lm(Petal.Length ~ Petal.Width, data=iris)

df_predict <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = 100)))

y_predict <- predict(m2_iris, newdata = df_predict)

df_predict <- df_predict %>% 
  mutate(y_predict = y_predict)

print(df_predict)


iris %>% 
  ggplot(aes(x = Petal.Width, 
             y = Petal.Length, 
             colour = Species)) +
  geom_point() +
  geom_line(data = df_predict, aes(y = y_predict),
            color = "black") 
            
#END 
