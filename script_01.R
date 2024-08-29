# test codes -------------------------------------
x <- rnorm(100, mean=0, sd=1)
#estimate mean
mean(x)
#estimate sd
sd(x)

# create vector -----------------------------------------------------------

x <- c(1,3,4,8)
x

#character
x <- c("a", "b", "c")
x

#logical
x <- c(TRUE, FALSE, FALSE)
x

#sequence of numbers
x <- 1:5
x

#eplicate same numbers or characters
x <- rep(2, 5) #replicate 2 five times
x

#replicate number or character
x <- rep("a", 5)

#use seq() function to sequence number or character
x <- seq(1,10, by=2)
x

x<- seq(1,5, by=0.1)
x

x<- seq(1,5, length=7)
x

# check x -----------------------------------------------------------------
class(x)
typeof(x)
attributes(x)
length(x)
sum(x)
mean(x)

y <- c(1L, 2L)
class(y)


# element ID --------------------------------------------------------------
#access to the first element
x[1]

x[c(1,3)]


# equation ----------------------------------------------------------------
# creating a vector
x <- c(2,2,3,2,5)
x

# ex.1a equal
x == 2

# ex.1b larger than
x > 2 

#You can access elements that suffice the specified condition using brackets, for example:
# ex.2a equal
x[x == 2]

# ex.2b larger than
x[x > 2]

#Using which(), you can see which elements (i.e., #) matches what you need:
# ex.3a equal
which(x == 2) # returns which elements are equal to 2

# ex.3b larger than
which(x > 2)


# Matrix ------------------------------------------------------------------
#Create Matrix
#Matrix is a set of elements (single data type) that are organized into rows and columns:
  
#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3, byrow = T)
x


# Sum functions ----------------------------------------------------------
rowSums(x)
colSums(x)

# Access ----------------------------------------------------------
x[1 ,2] #the first row and second column
x[2, ] #the second row
x[c(2,3), ] # row 2 and 3


# data frame --------------------------------------------------------------
# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0


colnames(df0)

df0$TSS

df0[1,] # access row #1

df0[c(2,4),] # access row #2 and 4


#END