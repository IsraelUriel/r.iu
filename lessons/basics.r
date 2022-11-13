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

# Dataframes

# Vector from 10 to 21
x <- 10:21
# Function letters from index
y <- letters[x]
y

# Definition of vectors
name <- c("Mercury", "Venus", "Earth", 
          "Mars", "Jupiter", "Saturn", 
          "Uranus", "Neptune")
type <- c("Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", 
          "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 
              11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 
              0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
planets_df <- data.frame(name, type, diameter, rotation, rings)
planets_df

# Check the structure of planets_df
str(planets_df)

# Check the head of planets_df
head(planets_df)

# Print out diameter of Mercury (row 1, column 3)
planets_df[1,3]

# Print out data for Mars (entire fourth row)
planets_df[4,]

# Select first 5 values of diameter column
planets_df[1:5, "diameter"]

# Select the rings variable from planets_df
rings_vector <- planets_df$rings

# Print out rings_vector
rings_vector

# Adapt the code to select all columns for planets with rings
planets_df[rings_vector, ]

# Select planets with diameter < 1
subset(planets_df, diameter < 1)

# Dataframe Functions

a <- c(100, 10, 1000)
order(a)
a[order(a)]

# Use order() to create positions
positions <-  order(planets_df$diameter)

# Use positions to sort planets_df
planets_df[positions,]

# More Examples

df <- data.frame(age=10:21, 
                 group=letters[x]
                )
df

# Add Column
sex <- c("H","M","H","M","H","H","M","M","H","M","H","M")
df$sex = sex

# Delete Column
df$sex = NULL

df

# Select
df$group
df$age[6:10]

# Lists

# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# Construct list with these different elements:
my_list <- list(my_vector, my_matrix)

# Adapt list() call to give the components names
names(my_list) <- c("vec", "mat")

my_list

a_list <- list(vector = my_vector, matrix = my_matrix)
a_list

a_list$vector
a_list$vector[2:4]
a_list[[2]][3]

# Other Examples

list_2 <- list(string = "Israel", 
              numeric = 3,
              vector = c(4, 7, 9),
              matrix = matrix(1:9, nrow = 3, ncol = 3),
              list_3 = list(a = "Hello", b = "Uriel")
)
class(list_2)
list_2
