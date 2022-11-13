# Author: Israel Uriel
# Title: R Basics
# Date: 11.November.2022


# Comment

# Creating a Variable
i <- "Alo Israel"
print(i)

# Assigning Value to a Variable

i = "Alo Uriel"
# Another way to print ...
i

# Class of a Variable
class(i)

# An addition
5 + 5 
# A subtraction
5 - 5 
# A multiplication
3 * 5
# A division
(5 + 5) / 2 
# Exponentiation
2 ^ 5
# Modulo
28 %% 6

# Arithmetic Example & Another way to print ():
x <- 5
y <- 10
(r <- ((x - y)**2)/(x+y))

# Vectors:

numeric_vector <- c(1, 2, 3)
character_vector <- c("a", "b", "c")

is.vector(numeric_vector)
typeof(numeric_vector)
class(numeric_vector)
length(numeric_vector)

# Populate Vector Via Sequences
(a <- seq(from=0, to = 10, by = 2))
(b <- seq(0,10,3))
(c <- rep(5, times=5))
(d <- rep(5,5))

# Assign days as names of numeric_vector
names(numeric_vector) <- c("One", "Two", "Three")
numeric_vector

# Arithmetic with Vectors Example:
A_vector <- c(1, 2, 3)
B_vector <- c(4, 5, 6)

names_vector <- c("One","Two","Three")
names(A_vector) <- names_vector
names(B_vector) <- names_vector
# Take the sum of A_vector and B_vector
total_vector <- A_vector + B_vector

# Print out total_vector
total_vector

# Sum of Elements of a Vector
total_vector_A <- sum(A_vector)
total_vector_B <- sum(B_vector)

total_vector_A
total_vector_B

# Mean
mean(B_vector)

# Compare
total_vector_A > total_vector_B

# Get a Member of a Vector (Indexing Starts in 1)
A_vector[3]

# Get A Selection Within a Vector
selected_members_vector <- A_vector[c(2,3)]
selected_members_vector

selected_members_vector <- A_vector[c(1:3)]
selected_members_vector

selection_vector <- A_vector >= 2
selected_members_vector <- A_vector[selection_vector]
selected_members_vector

