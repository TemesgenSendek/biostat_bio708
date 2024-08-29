#Exercise_1
#Vector ------------------------------------------------
#Create three numeric vectors with length 3, 6 and 20, respectively. Vectors must be created using at least two different functions in R.
x <- c(1,4,8)
x; class(x)
x <- 1:6
x; class(x)
x <- seq(0,77.9, length=20)
x; class(x)

#Create three character vectors with length 3, 6 and 20, respectively. Vectors must be created using at least two different functions in R.
x <- c("x", "y", "z")
x; class(x)
x <- rep("k", 6)
x; class(x)
x <- sample(letters, 20)
x;class(x)

#Copy the following script to your R script and perform the following analysis:
set.seed(1)
x <- rnorm(100)
x

#Identify element IDs of x that are greater than 2.0
which(x>2)
#Identify element values of x that are greater than 2.0
x[x>2]



#Matrix ------------------------------------------------
#Create a numeric matrix with 4 rows and 4 columns. Each column must contain identical elements.
x <- matrix(rep(1:4, each=4), nrow = 4, ncol = 4) 
x; class(x); dim(x)

#Create a numeric matrix with 4 rows and 4 columns. Each row must contain identical elements.
x <- matrix(rep(1:4, each=4), nrow = 4, ncol = 4, byrow = TRUE) 
x; class(x); dim(x)

#Create a character matrix with 4 rows and 4 columns. Each column must contain identical elements.
x <- matrix(rep(letters[1:4], each=4), nrow = 4, ncol = 4) 
x; class(x); dim(x)

#Create a character matrix with 4 rows and 4 columns. Each row must contain identical elements.
x <- matrix(rep(letters[1:4], each=4), nrow = 4, ncol = 4, byrow = TRUE) 
x; class(x); dim(x)

#Copy the following script to your R script and perform the following analysis:
set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)
x

#Identify element IDs of x that are greater than 2.0 (specify row and column IDs)
which(x>2, arr.ind = T)
round(x, 2) # to check in the matrix

#Identify element values of x that are greater than 2.0 and calculate the mean.
x[x>2]
mean(x[x>2])

#Data Frame ------------------------------------------------
#Create a data frame of 3 variables with 10 elements 
#(name variables as x, y and z. x must be character while y and z must be numeric.
Weigh.10 <- data.frame(x = c("Scarlet","George", "Porter", "English", "Robert", "Ali", "Emmalynn", "Valentine", "Kennedy", "Clay"),
                y = rep((1:2), 5),
                z = round(seq(50,70, length=10),2)
                )
Weigh.10

#Check the data structure (higher-level) of x, y and z
str(Weigh.10)
dim(Weigh.10)
colnames(Weigh.10)
Weigh.10$x
Weigh.10[,1]
Weigh.10$y
Weigh.10[,2]
Weigh.10$z
Weigh.10[,3]

rownames(Weigh.10)
Weigh.10[1,]
Weigh.10[7,]
Weigh.10[10,]

#Copy the following script to your R script and perform the following analysis:
#Calculate the means of temperature and abundance for states VA and NC separately.

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)
df0

mean.temp <- tapply(df0$temperature, df0$state, mean)
mean.temp

mean.abund <- tapply(df0$abundance, df0$state, mean)
mean.abund

#extra exercise
with(df0, temperature[state == "VA"])
with(df0, temperature[state == "NC"])

with(df0, tapply(temperature, state, mean))
with(df0, tapply(abundance, state, mean))


# END
