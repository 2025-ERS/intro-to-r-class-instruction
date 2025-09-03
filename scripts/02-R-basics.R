# --------------------------HEADER ----------------
# title: "02-R-basics.R"
# author: "Han Olff"
# date: "2025-8-22"
# description: "Some basics of R"


# -------------------------- 01 SETUP ----------------
# clear everything in working memory 
rm(list = ls())
# Make sure that you have the renv package installed. If not, install it using install.packages("renv")
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
# Load the renv package
library(renv)
# restore the libraries in the right verions from the lock file 
renv::restore() 
# Load the libraries that you will use in this script. Only load the libraries that you will actually use!
library(tidyverse) # load the tidyverse libraries, including readr and ggplot2
library(lubridate) # for working with dates

# -------------------------- 02 Variables, vectors  ----------------
# Variables are used to store values, for example the number 3.14 in a variable called class_size (number of students in this class)
class_size <- 28 # assign the value 28 to the variable class_size
print(class_size) # print the value of the variable class_size

# You can also use the equals sign = for assignment, but the arrow <- is better in R
# logical comparison
class_size == 30 # is the value of class_size equal to 30? This will return TRUE or FALSE
class_size != 30 # is the value of class_size not equal to 30?
class_size > 20 # is the value of class_size greater than 20?
class_size < 20 # is the value of class_size less than 20?
class_size >= 28 # is the value of class_size greater than or equal to 28?
class_size <= 28 # is the value of class_size less than or equal to 28?
# You can use variables in calculations
double_class_size <- class_size * 2 # double the value of class_size and store it in a new variable double_class_size
double_class_size # print the value of double_class_size
(2+3-5+26)/5

# You can also use variables in calculations with other variables
half_class_size <- class_size / 2 # halve the value of class_size and store it in a new variable half_class_size
half_class_size # print the value of half_class_size
total_students <- class_size + double_class_size + half_class_size # sum the values of class_size, double_class_size and half_class_size and store it in a new variable total_students
total_students # print the value of total_students
# You can change (update) the value of a variable
class_size
class_size <- 30 # change the value of class_size to 30
class_size # print the value of class_size

# You can also create   vectors, which are ordered collections of values
# A vector can contain numbers, characters, or logical values
# You can create a vector using the c() function, which stands for "combine"
# Create a numeric vector with the numbers 1, 2, 3, 4
numeric_vector <- c(1, 2, 3, 4) 
print(numeric_vector) # print the value of numeric_vector
# you can calculate with vectors same as you did with above
numeric_vector*2
class(numeric_vector)

# -------------------------- 03 Lists, data frames and tibbles ----------------
# a list is a collection of objects, which can be of different types and lengths
# such as text, numbers or boolean variables
# Create a simple list
my_list <- list(
  name = "Elephant",
  weight = 5400,
  is_endangered = TRUE,
  colors = c("gray", "white")
)
my_list
# in practice, we typically read a dataframe from a file, not create it ourselves in a script like this

my_list$name # print the value of the name element in my_list
my_list$weight # print the value of the weight element in my_list

# A data frame is a table-like structure, where each column can be of a different type (numeric, character, logical)
# Create a simple data frame
data_herbivores <- data.frame(
  id = c(1, 2, 3),
  species = c("Elephant", "Buffalo", "Impala"),
  bodymass_kg = c(5400, 800, 50),
  is_ungulate = c(FALSE, TRUE, TRUE)
)
data_herbivores # print  data_herbivores dataframe
names(data_herbivores) # print the names of the variables in data_herbivores
class(data_herbivores) # print the class of data_herbivores
data_herbivores$species # print the species column in data_herbivores (which becomes a vector)
# selecting rows and columns of a dataframe using the dplyr library plus the pipe operator |>
data_herbivores |> dplyr::select(species, bodymass_kg) # select the species and bodymass_kg columns from data_herbivores
data_herbivores |> dplyr::filter(bodymass_kg > 100) # filter the rows where bodymass_kg is greater than 100

# A tibble is a modern version of a data frame, which is part of the tidyverse
# it shows the types of the variables, and only the first 10 rows and columns that fit on the screen
data_herbivores |> tibble::as_tibble() 

# -------------------------- 04 Data types - numeric and logical ----------------
# There are several data types in R, the most common are:
# Numeric: numbers with or without decimal points
num_var <- 3.14 # assign the value 3.14 to the variable num_var
num_var # print the value of num_var
class(num_var) # print the class of num_var
# Integer: whole numbers
int_var <- 42L # assign the value 42 to the variable int_var, the L indicates that it is an integer
int_var # print the value of int_var
class(int_var) # print the class of int_var
# Character: text strings
char_var <- "Hello, R!" # assign the value "Hello, R!" to the variable char_var
char_var # print the value of char_var
class(char_var) # print the class of char_var
# Logical: TRUE or FALSE values
log_var <- TRUE # assign the value TRUE to the variable log_var
log_var # print the value of log_var
class(log_var) # print the class of log_var
# You can also use FALSE
log_var2 <- FALSE # assign the value FALSE to the variable log_var2
log_var2 # print the value of log_var2
class(log_var2) # print the class of log_var2


# -------------------------- 05 Data types - dates and time ----------------
date_var <- as.Date("2023-01-12", format="%Y-%m-%d") # assign the value "2023-01-01" to the variable date_var
?strptime # check the help file of this function for the format codes
date_var # print the value of date_var
class(date_var) # print the class of date_var

# But it is generally easier to use the lubridate package, 
# when working with dates without worrying about the format strings
date_var2 <- lubridate::ymd("2023-12-18") # assign the value 18-dec-2023 to the variable date_var2 using lubridate
date_var2 # print the value of date_var2
class(date_var2) # print the class of date_var2
# You can also use lubridate to work with dates in different formats
date_var3 <- lubridate::dmy("18-Dec-2023") # assign the value 18-dec-2023 to the variable date_var3 using lubridate
date_var3 # print the value of date_var3
class(date_var3) # print the class of date_var3
# You can also use lubridate to work with dates in different formats
date_var4 <- lubridate::mdy("Dec 18, 2023") # assign the value 18-dec-2023 to the variable date_var4 using lubridate
date_var4 # print the value of date_var4
class(date_var4) # print the class of date_var4
# You can also use lubridate to work with dates combined with time in different formats
date_var5 <- lubridate::ymd_hms("2023-12-18 14:30:00") # assign the value "2023-12-18 14:30:00" to the variable date_var5 using lubridate
date_var5 # print the value of date_var5
# date variables can be used in calculations 
date_var6 <- date_var5 + lubridate::days(10) # add 10 days to date_var5 and store it in a new variable date_var6
date_var6 # print the value of date_var6
# default time zone is UTC (similar to GMT, Greenwich time)
# find the time zone name  for Nairobi using Olsen names
grep("Nairobi", OlsonNames(), value = TRUE)
date_var7 <- lubridate::ymd_hms("2023-12-18 14:30:00",tz="Africa/Nairobi") # assign the value "2023-12-18 14:30:00" to the variable date_var5 using
date_var7

# -------------------------- 06 functions ----------------
# R has many built-in functions, for example the sqrt() function to calculate the square root of a number
base::pi # print the value of pi, a function without arguments
base::sqrt(16) # calculate the square root of 16
x<-base::c(1,2,3,4,5)
data
base::mean(data) # calculate the mean of the numbers 1,2,3,4,5
stats::sd(data) # calculate the standard deviation of the numbers 1,2,3,4,5
# typically for functions from libraries base and stat we omit the library, but keep it for all others!

# Also all stastistical analysis are implemented as functions, for example 
# then the function returns the result of the analysis

model1<-lm(y~x) # linear model of y as a function of x
print(model1) # print the result of the model
class(model1)
print(unclass(model1))
model1$coefficients # print the coefficients of the model (slope and intercept)
model1$coefficients[2] # print the slope of the model
model1$coefficients[1] # print the intercept of the model

summary(model1) # print the summary of the model  

print(lm) # print the function lm (linear model) to see its arguments
?lm # print the help file for the function lm, more usefull for the arguments

# define yourself a function to calculate the area of a circle
circle_area <- function(radius) { # define a function called circle_area that takes one argument, the radius
  area <- pi * radius^2 # calculate the area of the circle using the formula A = πr^2
  return(area) # return the value of area
}
# use the function to calculate the area of a circle with radius 5
circle_area(5) # call the function circle_area with the argument 5

# a more complex function that calculates some statistics for a numeric vector x
# specify is that the default value for the argument na.rm is TRUE, so that it removes NA values by default
# but you can also set it to FALSE if you want to keep NA values in the calculation
# Define the function
my_stats <- function(x, na.rm = TRUE) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input must be numeric!")
  }
  
  # Remove NA values if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Compute statistics
  n <- base::length(x)            # not that functions can use other functions from other libraries
  mean_val <- base::mean(x)
  median_val <- stats::median(x)
  sd_val <- stats::sd(x)
  range_val <- baserange(x)
  
  # Return a list with named components
  result <- list(
    n = n,
    mean = mean_val,
    median = median_val,
    sd = sd_val,
    min = range_val[1],
    max = range_val[2]
  )
  
  return(result)
}
# How to Use It
# Create a numeric vector with some NA values
data <- c(2, 5, 7, 8, NA, 10)
my_stats(data)
# You can also use the function with the na.rm argument set to FALSE
my_stats(data, na.rm = FALSE) # this will keep the NA values in the calculation
# If you try to use the function with a non-numeric vector, it will give an
# error message
my_stats(c("a", "b", "c")) # this will give an error message because the input is not numeric

# -------------------------- 07 Objects and methods  ----------------
# In R, everything (a number, vector, output of an analysis) is an object, and objects can have different classes
# You can check the class of an object using the class() function
num_var <- 3.14
class(num_var) # print the class of num_var
x<-c(1,2,3,4,5)
class(x) 
y<-c(2,4,1,6,5)
class(y)
data<-base::data.frame(x,y)
print(data)
class(data)
linear_model<-stats::lm(y~x)
class(linear_model)

# Functions can have different methods, which are specific implementations of the function depending on the of class of the object that they are applied to. 
# For example, the summary() function has different methods for different classes of objects
summary(x) # summary method for numeric objects, applies the summary.default() method
summary(data) # summary method for data.frame objects, applies the summary.data.frame() method
summary(linear_model) # summary method for lm objects, applies the summary.lm() method
# You can see the different methods for a function using the methods() function
methods("summary") # print the methods for the summary function
methods("plot") # print the methods for the plot function
