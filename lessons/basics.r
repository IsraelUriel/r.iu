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
(b <- seq(0,10,2))
(c <- rep(5, times=5))
(d <- rep(5,5))

# Bindings
(vector_ab <- cbind(a,b))
(vector_cd <- rbind(a,b))

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

# Matrix

# Construct a matrix with 3 rows that contain the numbers 1 up to 9
matrix(1:9, byrow = TRUE, nrow=3)

vector_a <- seq(1,5,1)
vector_b <- seq(6,10,1)
vector_c <- seq(11,15,1)

vector_union <- c(vector_a, vector_b, vector_c)
vector_union

matrix_v <- matrix(vector_union, byrow = TRUE, nrow = 3)
matrix_v

row_names <- c("vector_a", "vector_b", "vector_c")
col_names <- c("one","two","three","four","five")

rownames(matrix_v) <- row_names
colnames(matrix_v) <- col_names

matrix_v

# Other Way To Name Matrix
matrix_v <- matrix(vector_union, byrow = TRUE, nrow = 3, dimnames = list(row_names, col_names))
matrix_v

# Matrix Dimension
dim(matrix_v)

# Arithmetic Operations
vector_matrix_total <- rowSums(matrix_v)
vector_matrix_total

matrix_v / 5
matrix_v * 5
(mean(matrix_v))


# Combines
matrix_ab <- c(vector_a, vector_b)
matrix_bc <- c(vector_b, vector_c)

row_names_ab <- c("matrix_ab", "matrix_bc")
col_names_ab <- c("one","two","three","four","five","six","seven","eight","nine","ten")
matrix_ab <- matrix(cbind(matrix_ab, matrix_bc), byrow = TRUE, nrow = 2, dimnames = list(row_names_ab, col_names_ab))

matrix_ab 

total_matrix_rows = rowSums(matrix_ab)
total_matrix_cols = colSums(matrix_ab)
                       
total_matrix_rows
total_matrix_cols

# Selections

matrix_v

m_sel_col_2 <- matrix_v[,2]
m_sel_col_2

m_sel_col_2_4 <- matrix_v[,2:4]
m_sel_col_2_4

v_a_selection <- ((vector_a %% 2) == 0) & (vector_a > 2)
v_a_selection 

vector_r <- vector_a[v_a_selection]
vector_r


