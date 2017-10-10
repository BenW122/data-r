### Part 1: first steps

# Using R as calculator
2 + 2
3 + 5 * 2
(3 + 5) * 2

# Assigning (and working with) variables
a <- 5
b <- 4
a + b
a * b

8 -> c

### Part 2: data types

# Types: logical, numeric, character, (complex)
# check using functions()

is.logical(TRUE)
is.character("Data Science")
is.numeric(3.1415)
is.numeric(a-b)

is.character(4711)
is.numeric("We love R!")

# Vectors
v <- c(4, 7, 1, 1, 0, 8, 1, 5)
w1 <- v[1:4]
w2 <- v[-1]
w3 <- v[c(1,3,8)]
w4 <- v[c(1:3,5:7)]
w5 <- v[-c(4,8)]

# Matrix (two-dimensional array)
m1 <- matrix(v, nrow=2)
m2 <- matrix(v, nrow=2, byrow=TRUE)
m3 <- matrix(v, nrow=4)

m3[,]
m3[,1]
m3[,2]
m3[1,]
m3[2,]

# Lists
k <- list(5, "AAPL", 3, "MSFT", TRUE)
k[[2]]

# Functions
multiply_by_two <- function(x) {
  y <- x * 2
  return(y)
}
multiply_by_two(4)

# Data frames
f <- data.frame(m2)
names(f)
f$X2
names(f) <- c("A", "B", "C", "D")
f$C

### Part 3: the power of "apply"

# apply
apply(m1, 1, mean)
apply(m1, 2, mean)
apply(m1, 2, sd)

# sapply
sapply(v, function(x) x*2)
sapply(k, is.character)
