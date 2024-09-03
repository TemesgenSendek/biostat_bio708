
library(tidyverse)
data("iris")
iris
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

#Reshape ----
##pivot_wider() -----
#reshape a data frame to a wide format

iris_wide <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from
print(iris_wide)

#pivot_longer() -----
#reshape a data frame to a long format
iris_long <- iris_wide %>% 
  pivot_longer(cols = c("setosa", "versicolor", "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"
print(iris_long)


#Group Operation
#group_by() & summarize(): group-by-group operation. summarize() does not retain individual rows.

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length))


# grouping by "Species", then take means & SD "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sd_sl = sd(Sepal.Length))


#group_by() & mutate(): group-by-group operation. mutate() retains individual rows along with summary columns. Do not forget ungroup() to avoid errors in following operations.

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()

#Join
#left_join(): merge data frames based on column(s)

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")

# matching by a single column
## left join by "Species": one to many
df3 <- tibble(Species = c("A", "A", "B", "C"),
              y = c(4, 5, 6, 7))

left_join(x = df1,
          y = df3,
          by = "Species")


# matching by a single column
## left join by "Species": one to missing
df4 <- tibble(Species = c("A", "A", "C"),
              y = c(4, 5, 7))

left_join(x = df1,
          y = df4,
          by = "Species")


# matching by multiple columns
## one to one
df5 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df5,
          by = c("Species", "x"))


# matching by multiple columns
## one to many
df6 <- tibble(Species = c("A", "A", "B", "C"),
              x = c(1, 1, 2, 3),
              z = c("cool", "cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))


# matching by multiple columns
## one to missing
df6 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 4),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))


#Exercise -----

# Visualization ----
#ggplot2

##Point----
#geom_point(): Add a point layer
# basic plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()
# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

##Line ----
#geom_line() : Add a line layer
# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)
# basic plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()

##Histogram -----
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

##Boxplot ----
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
             fill = Species)) +
  geom_boxplot()
# change fill by "Species", but consistent color
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkgrey")
