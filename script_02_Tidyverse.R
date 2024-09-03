#Tidyverse
library(tidyverse)
data("iris")
head(iris)
iris <- as_tibble(iris)
iris

#Data Manipulation ------------------------
#Data Format
#dplyr:: and tidyr:: for data manipulations that include: row/column manipulations, group operation, reshape, and more.

##Row Manipulation --------------
###filter(): select/remove rows ----

# single match "=="
filter(iris, Species == "virginica")
# multiple match "%in%"
filter(iris, Species %in% c("virginica", "versicolor"))
# except "!="
filter(iris, Species != "virginica")
# except multiple "!(x %in% c("a", "b"))
filter(iris, !(Species %in% c("virginica", "versicolor")))
# greater than ">"
filter(iris, Sepal.Length > 5)
# equal & greater than ">="
filter(iris, Sepal.Length >= 5)
# less than "<"
filter(iris, Sepal.Length < 5)
# equal & less than "<="
filter(iris, Sepal.Length <= 5)

###arrange() : arrange the order of rows ----
# arrange in an ascending order
arrange(iris, Sepal.Length)
# arrange in an descending order
arrange(iris, desc(Sepal.Length))


##Column Manipulation -----------------
###select(): select/remove column(s) ----
# select one column
select(iris, Sepal.Length)
# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))
# remove one column
select(iris, -Sepal.Length)
# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))
# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))
# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))

###mutate(): add column(s) ----
x <- 1:150
head(mutate(iris, x = x, .before = 1))


#Piping -------------
#%>% (pipe) allows sequential operations of multiple functions (shortcut: Ctr + Shift + M). The pipe passes the object to the following function as the first argument.

# the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)
# piping
iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)


#Reshape ----------------------------------------
##pivot_wider() -----
#reshape a data frame to a wide format

iris_wide <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from
print(iris_wide)

##pivot_longer() -----
#reshape a data frame to a long format

iris_long <- iris_wide %>% 
  pivot_longer(cols = c("setosa", "versicolor", "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_long)

#End---part 1