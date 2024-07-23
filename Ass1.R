# Load library 
library(ggplot2)
library(testthat)
library(MASS)

# Define the custom function
# A.at least two inputs (1 required and 1 default/optional)
# shapiro_wilk_test <- function(data, disp_qqplot = FALSE) {
shapiro_wilk_test <- function(...) {
  
  # B.Input validation
  # Check the number of parameters
  args <- list(...)
  
  if (length(args) < 1 || length(args) > 2) {
    stop("The function requires 1 or 2 parameters: data (required) and disp_qqplot (optional).")
  }
  
  # Assign arguments to variables
  data <- args[[1]]
  disp_qqplot <- ifelse(length(args) == 2, args[[2]], FALSE)
  
  # ---------------------------------------------------------
  if (is.null(data)) stop("Input data is null")
  if (!is.numeric(data)) stop("Data must be a numeric vector.")
  if (any(is.na(data))) stop("Data contains NA values.")
  if (any(is.infinite(data))) stop("Data contains infinite values.")
  if (length(data) < 3) stop("Data must contain at least three values.")
  

  # Order statistics
  n <- length(data)
  sorted_data <- sort(data)
  
  # Mean of the data
  x_bar <- mean(data)
  
  # Coefficients for Shapiro-Wilk test
  
  a <- qnorm((1:n - 0.375) / (n + 0.25))
  m <- mean(a)
  a <- (a - m) / sd(a)
  
  # Calculate W
  numerator <- sum(a * sorted_data)^2
  denominator <- sum((data - x_bar)^2)
  
  W <- numerator / denominator
  
  
  # Print the result
  print(W)
  
  # qq plot 
  if (disp_qqplot) {
    # QQplot of normally distributed values
    qqnorm(data)
    # Add qqline to plot
    qqline(data, col = "darkgreen")
    
  }
  
  # Return the result
  return(W)
}

##--------------------------------------------------------------------
# Basic/simple Tests
# Test 1: Normal data without QQ-plot
sample_data1 <- c(4.2, 5.3, 6.1, 7.4, 8.0, 4.8, 5.9, 6.2, 7.5, 5.7)
result1 <- shapiro_wilk_test(sample_data1)
result1 <- shapiro_wilk_test(sample_data1, disp_qqplot = TRUE)

# Test 2: Normal data with QQ-plot
set.seed(500)
sample_data2 <- rnorm(5000)
result2 <- shapiro_wilk_test(sample_data2, disp_qqplot = TRUE)


##--------------------------------------------------------------------
# Catches errors 
# Test 3: Data with NA values
sample_data3 <- c(1.2, 2.3, NA, 4.5)
tryCatch({
  result3 <- shapiro_wilk_test(sample_data3)
}, error = function(e) {
  print(paste("Error: ", e$message))
})

# Test 4: Data with infinite values
sample_data4 <- c(1.2, 2.3, Inf, 4.5)
tryCatch({
  result4 <- shapiro_wilk_test(sample_data4)
}, error = function(e) {
  print(paste("Error: ", e$message))
})

# Test 5: Data with wrong format (non-numeric)
sample_data5 <- c("a", "b", "c")
tryCatch({
  result5 <- shapiro_wilk_test(sample_data5)
}, error = function(e) {
  print(paste("Error: ", e$message))
})

# Test 6: Data with less than three values
sample_data6 <- c(1.2, 2.3)
tryCatch({
  result6 <- shapiro_wilk_test(sample_data6)
}, error = function(e) {
  print(paste("Error: ", e$message))
})


##--------------------------------------------------------------------
# Develop the tests 
# usethis::use_testthat()
test_that("Test3 NA values", {
  sample_data3 <- c(1.2, 2.3, NA, 4.5)
  expect_error(shapiro_wilk_test(sample_data3), "Data contains NA values.") 
})

test_that("Test4 infinite values", {
  sample_data4 <- c(1.2, 2.3, Inf, 4.5)
  expect_error(shapiro_wilk_test(sample_data4), "Data contains infinite values.") 
})

test_that("Test5 wrong format", {
  sample_data5 <- c("a", "b", "c")
  expect_error(shapiro_wilk_test(sample_data5), "Data must be a numeric vector.") 
})

test_that("Test6 error", {
  sample_data6 <- c(1.2, 2.3)
  expect_error(shapiro_wilk_test(sample_data6), "Data must contain at least three values.") 
})


